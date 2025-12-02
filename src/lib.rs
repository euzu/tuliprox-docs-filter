#![allow(clippy::empty_docs)]

mod item_field;
mod item_type;

use enum_iterator::all;
use pest_derive::Parser;
use pest::iterators::Pair;
use pest::Parser;
use serde::{Serialize};
use wasm_bindgen::prelude::*;
use crate::item_field::ItemField;
use crate::item_type::ItemType;

#[derive(Debug, Clone, Serialize)]
pub struct CompiledRegex {
    pub value: String,
}

impl PartialEq for CompiledRegex {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

#[derive(Parser)]
#[grammar_inline = r#"
WHITESPACE = _{ " " | "\t" | "\r" | "\n"}
field = { ^"group" | ^"title" | ^"name" | ^"url" | ^"input" | ^"caption"}
and = { ^"and" }
or = { ^"or" }
not = { ^"not" }
regexp = @{ "\"" ~ ( "\\\"" | (!"\"" ~ ANY) )* ~ "\"" }
type_value = { ^"live" | ^"vod" | ^"movie" | ^"series" }
type_comparison = { ^"type" ~ "=" ~ type_value }
field_comparison_value = _{ regexp }
field_comparison = { field ~ "~" ~ field_comparison_value }
comparison = { field_comparison | type_comparison }
bool_op = { and | or }
expr_group = { "(" ~ expr ~ ")" }
basic_expr = _{ comparison | expr_group }
not_expr = _{ not ~ basic_expr }
expr = {
  not_expr ~ (bool_op ~ expr)?
  | basic_expr ~ (bool_op ~ expr)*
}
stmt = { expr ~ (bool_op ~ expr)* }
main = _{ SOI ~ stmt ~ EOI }
"#]
struct FilterParser;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub enum UnaryOperator {
    Not,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub enum BinaryOperator {
    And,
    Or,
}

impl BinaryOperator {
    const OP_OR: &'static str = "OR";
    const OP_AND: &'static str = "AND";
}

impl std::fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", match *self {
            Self::Or => Self::OP_OR,
            Self::And => Self::OP_AND,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Filter {
    Group(Box<Filter>),
    FieldComparison(ItemField, CompiledRegex),
    TypeComparison(ItemField, ItemType),
    UnaryExpression(UnaryOperator, Box<Filter>),
    BinaryExpression(Box<Filter>, BinaryOperator, Box<Filter>),
}

impl std::fmt::Display for Filter {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::FieldComparison(field, rewc) => {
                write!(f, "{} ~ \"{}\"", field, String::from(&rewc.value))
            }
            Self::TypeComparison(field, item_type) => {
                write!(f, "{} = {}", field, match item_type {
                    ItemType::Live => ItemType::LIVE,
                    ItemType::Video => ItemType::MOVIE,
                    ItemType::Series => ItemType::SERIES, // yes series-info is handled as series in filter
                })
            }
            Self::Group(stmt) => {
                write!(f, "({stmt})")
            }
            Self::UnaryExpression(op, expr) => {
                let flt = match op {
                    UnaryOperator::Not => format!("NOT {expr}"),
                };
                write!(f, "{flt}")
            }
            Self::BinaryExpression(left, op, right) => {
                write!(f, "{left} {op} {right}")
            }
        }
    }
}

fn get_parser_item_field(expr: &Pair<Rule>) -> Result<ItemField, std::fmt::Error> {
    if expr.as_rule() == Rule::field {
        let field_text = expr.as_str();
        for item in all::<ItemField>() {
            if field_text.eq_ignore_ascii_case(item.to_string().as_str()) {
                return Ok(item);
            }
        }
    }
        // "unknown field: {}", expr.as_str()
    Err(std::fmt::Error{})
}

fn get_parser_regexp(expr: &Pair<Rule>) -> Result<CompiledRegex, std::fmt::Error> {
    if expr.as_rule() == Rule::regexp {
        let mut parsed_text = String::from(expr.as_str());
        parsed_text.pop();
        parsed_text.remove(0);
        let re = regex::Regex::new(parsed_text.as_str());
        if re.is_err() {
            //return create_tuliprox_error_result!(TuliproxErrorKind::Info, "cant parse regex: {}", regstr);
            return Err(std::fmt::Error{});
        }
        return Ok(CompiledRegex {
            value: parsed_text,
        });
    }
    //create_tuliprox_error_result!(TuliproxErrorKind::Info, "unknown field: {}", expr.as_str())
    Err(std::fmt::Error{})
}

fn get_parser_field_comparison(
    expr: Pair<Rule>
) -> Result<Filter, std::fmt::Error> {
    let mut expr_inner = expr.into_inner();
    match get_parser_item_field(&expr_inner.next().unwrap()) {
        Ok(field) => match get_parser_regexp(&expr_inner.next().unwrap()) {
            Ok(regexp) => Ok(Filter::FieldComparison(field, regexp)),
            Err(err) => Err(err),
        },
        Err(err) => Err(err),
    }
}

fn get_filter_item_type(text_item_type: &str) -> Option<ItemType> {
    if text_item_type.eq_ignore_ascii_case("live") {
        Some(ItemType::Live)
    } else if text_item_type.eq_ignore_ascii_case(ItemType::VOD)
        || text_item_type.eq_ignore_ascii_case(ItemType::VIDEO)
        || text_item_type.eq_ignore_ascii_case(ItemType::MOVIE)
    {
        Some(ItemType::Video)
    } else if text_item_type.eq_ignore_ascii_case("series") {
        Some(ItemType::Series)
    } else if text_item_type.eq_ignore_ascii_case("series-info") {
        // this is necessarry to avoid series and series-info confusion in filter!
        // we can now use series  for filtering series and series-info (series-info are categories)
        Some(ItemType::Series)
    } else {
        None
    }
}

fn get_parser_type_comparison(expr: Pair<Rule>) -> Result<Filter, std::fmt::Error> {
    let expr_inner = expr.into_inner();
    let text_item_type = expr_inner.as_str();
    let item_type = get_filter_item_type(text_item_type);
    item_type.map_or_else(|| Err(std::fmt::Error{}) /*create_tuliprox_error_result!(TuliproxErrorKind::Info, "cant parse item type: {text_item_type}") */,
                          |itype| Ok(Filter::TypeComparison(ItemField::Type, itype)))
}

macro_rules! handle_expr {
    ($bop: expr, $uop: expr, $stmts: expr, $exp: expr) => {{
        let result = match $bop {
            Some(binop) => {
                let lhs = $stmts.pop().unwrap();
                $bop = None;
                Filter::BinaryExpression(Box::new(lhs), binop.clone(), Box::new($exp))
            }
            _ => match $uop {
                Some(unop) => {
                    $uop = None;
                    Filter::UnaryExpression(unop.clone(), Box::new($exp))
                }
                _ => $exp,
            },
        };
        $stmts.push(result);
    }};
}

fn get_parser_expression(
    expr: Pair<Rule>,
    errors: &mut Vec<String>,
) -> Result<Filter, String> {
    let mut stmts = Vec::with_capacity(128);
    let pairs = expr.into_inner();
    let mut bop: Option<BinaryOperator> = None;
    let mut uop: Option<UnaryOperator> = None;

    for pair in pairs {
        match pair.as_rule() {
            Rule::field_comparison => {
                let comp_res = get_parser_field_comparison(pair);
                match comp_res {
                    Ok(comp) => handle_expr!(bop, uop, stmts, comp),
                    Err(err) => errors.push(err.to_string()),
                }
            }
            Rule::type_comparison => {
                let comp_res = get_parser_type_comparison(pair);
                match comp_res {
                    Ok(comp) => handle_expr!(bop, uop, stmts, comp),
                    Err(err) => errors.push(err.to_string()),
                }
            }
            Rule::comparison | Rule::expr => {
                match get_parser_expression(pair, errors) {
                    Ok(expr) => handle_expr!(bop, uop, stmts, expr),
                    Err(err) => return Err(err),
                }
            }
            Rule::expr_group => {
                match get_parser_expression(pair.into_inner().next().unwrap(), errors) {
                    Ok(expr) => handle_expr!(bop, uop, stmts, Filter::Group(Box::new(expr))),
                    Err(err) => return Err(err),
                }
            }
            Rule::not => {
                uop = Some(UnaryOperator::Not);
            }
            Rule::bool_op => match get_parser_binary_op(&pair.into_inner().next().unwrap()) {
                Ok(binop) => {
                    bop = Some(binop);
                }
                Err(err) => {
                    errors.push(format!("{err}"));
                }
            },
            _ => {
                errors.push(format!("did not expect rule: {pair:?}"));
            }
        }
    }
    if stmts.is_empty() {
        return Err(format!("Invalid Filter, could not parse {errors:?}"));
    }
    if stmts.len() > 1 {
        return Err(format!("did not expect multiple rule: {stmts:?}, {errors:?}"));
    }

    Ok(stmts.pop().unwrap())
}

fn get_parser_binary_op(expr: &Pair<Rule>) -> Result<BinaryOperator, std::fmt::Error> {
    match expr.as_rule() {
        Rule::and => Ok(BinaryOperator::And),
        Rule::or => Ok(BinaryOperator::Or),
        _ => Err(std::fmt::Error{})/*create_tuliprox_error_result!(
            TuliproxErrorKind::Info,
            "Unknown binary operator {}",
            expr.as_str()
        )*/,
    }
}

pub fn get_filter(
    filter_text: &str,
) -> Result<Filter, std::fmt::Error> {
    let source = filter_text;

    match FilterParser::parse(Rule::main, source) {
        Ok(pairs) => {
            let mut errors = Vec::new();
            let mut result: Option<Filter> = None;
            let mut op: Option<BinaryOperator> = None;
            for pair in pairs {
                match pair.as_rule() {
                    Rule::stmt => {
                        for expr in pair.into_inner() {
                            match expr.as_rule() {
                                Rule::expr => {
                                    match get_parser_expression(expr, &mut errors) {
                                        Ok(expr) => {
                                            match &op {
                                                Some(binop) => {
                                                    result = Some(Filter::BinaryExpression(
                                                        Box::new(result.unwrap()),
                                                        *binop,
                                                        Box::new(expr),
                                                    ));
                                                    op = None;
                                                }
                                                _ => result = Some(expr),
                                            }
                                        }
                                        Err(err) => errors.push(err),
                                    }
                                }
                                Rule::bool_op => {
                                    match get_parser_binary_op(&expr.into_inner().next().unwrap()) {
                                        Ok(binop) => {
                                            op = Some(binop);
                                        }
                                        Err(err) => {
                                            errors.push(err.to_string());
                                        }
                                    }
                                }
                                _ => {
                                    errors.push(format!("unknown expression {expr:?}"));
                                }
                            }
                        }
                    }
                    Rule::EOI => {}
                    _ => {
                        errors.push(format!("unknown: {}", pair.as_str()));
                    }
                }
            }

            if !errors.is_empty() {
                errors.push(format!("Unable to parse filter: {}", &filter_text));
                return Err(std::fmt::Error{}/*info_err!(errors.join("\n"))*/);
            }

            result.map_or_else(
                || {
                    Err(std::fmt::Error{})
                    /*create_tuliprox_error_result!(
                        TuliproxErrorKind::Info,
                        "Unable to parse filter: {}",
                        &filter_text
                    )*/
                },
                Ok,
            )
        }
        Err(_err) => Err(std::fmt::Error{}) /*create_tuliprox_error_result!(TuliproxErrorKind::Info, "{}", err)*/,
    }
}


#[wasm_bindgen]
pub fn get_filter_js(filter_text: &str) -> Result<String, String> {
    match crate::get_filter(filter_text) {
        Ok(filter) => {
            serde_json::to_string(&filter)
                .map_err(|e| format!("Err: {}", e.to_string()))
        }
        Err(_) => Err("Err: Failed to parse filter".to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    pub fn test_filter() {
        let flt= r#"(Group ~ "^FR.*") AND NOT (Group ~ ".*XXX.*" OR Group ~ ".*SERIES.*" OR Group ~ ".*MOVIES.*")"#;
        if let Ok(filter)  = crate::get_filter(flt) {
            if let Ok(serde_value) = serde_json::to_value(filter) {
                println!("{serde_value:?}");
            }
        }
    }
}