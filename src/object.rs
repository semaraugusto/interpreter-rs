use crate::ast;
use crate::environ::Environment;
use crate::errors::*;
use std::fmt;

fn print_type_of<T>(_: &T) {
    println!("{}", std::any::type_name::<T>())
}

#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    ReturnValue(Box<Object>),
    Error(String),
    Function(Function),
}

const INTEGER_STR: &str = "INTEGER";
const NULL_STR: &str = "NULL";
const BOOL_STR: &str = "BOOLEAN";
const RETURN_STR: &str = "RETURN";
const ERROR_STR: &str = "ERROR";
const FUNCTION_STR: &str = "FUNCTION";
const TRUE_OBJ: Object = Object::Boolean(true);
const FALSE_OBJ: Object = Object::Boolean(false);
const NULL_OBJ: Object = Object::Null;

impl Object {
    pub fn obj_type(&self) -> String {
        match self {
            Object::Integer(_) => INTEGER_STR.to_string(),
            Object::Boolean(_) => BOOL_STR.to_string(),
            Object::Null => NULL_STR.to_string(),
            Object::ReturnValue(_) => RETURN_STR.to_string(),
            Object::Error(_) => ERROR_STR.to_string(),
            Object::Function(_) => FUNCTION_STR.to_string(),
        }
    }
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(i) => i.to_string(),
            Object::Boolean(b) => b.to_string(),
            Object::ReturnValue(obj) => obj.inspect(),
            Object::Null => NULL_STR.to_lowercase(),
            Object::Error(err) => ("ERROR: ".to_string() + err),
            Object::Function(function) => {
                format!("fn({:?}) {{\n{}\n}}", function.parameters, function.body)
            }
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Integer(i) => write!(f, "{}", i),
            Object::Boolean(b) => write!(f, "{}", b),
            Object::ReturnValue(obj) => write!(f, "{}", obj.inspect()),
            Object::Null => write!(f, "{}", NULL_STR.to_lowercase()),
            Object::Error(err) => write!(f, "{}", "ERROR: ".to_string() + err),
            Object::Function(function) => {
                write!(f, "fn({:?}) {{\n{}\n}}", function.parameters, function.body)
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    parameters: Vec<Box<ast::Expression>>,
    body: ast::BlockStatement,
    env: Environment,
}

fn evaluate(node: ast::Node, mut env: Environment) -> (Object, Environment) {
    println!("Evaluating: {}", node);
    print_type_of(&node);
    match node {
        ast::Node::Statement(stmt) => match stmt {
            ast::Statement::ReturnStatement { token, expr } => {
                let (obj, env) = evaluate(ast::Node::Expression(*expr.unwrap()), env);
                if let Object::Error(_) = obj {
                    return (obj, env);
                }
                (Object::ReturnValue(Box::new(obj)), env)
            }
            ast::Statement::LetStatement { token, name, value } => {
                let (obj, mut env) = evaluate(ast::Node::Expression(*value.unwrap()), env);
                if let Object::Error(_) = obj {
                    return (obj, env);
                }
                env.set(name.to_string(), obj.clone());
                (obj, env)
            }
            ast::Statement::ExpressionStatement { token, expr } => match *expr.unwrap() {
                ast::Expression::Identifier { token, value } => {
                    let id: Object;
                    (id, env) = eval_identifier(value.to_string(), env);
                    (id, env)
                }
                ast::Expression::IntegerLiteral { token: _, value } => {
                    (Object::Integer(value), env)
                }
                ast::Expression::Boolean { token: _, value } => (Object::Boolean(value), env),
                ast::Expression::PrefixExpression {
                    token: _,
                    operator,
                    right,
                } => {
                    let result_right: Object;
                    (result_right, env) = evaluate(ast::Node::Expression(*right.unwrap()), env);
                    if let Object::Error(_) = result_right {
                        return (result_right, env);
                    }
                    (evaluate_prefix_expression(operator, result_right), env)
                }
                ast::Expression::InfixExpression {
                    token: _,
                    left,
                    operator,
                    right,
                } => {
                    let result_left;
                    (result_left, env) = evaluate(ast::Node::Expression(*left.unwrap()), env);
                    if let Object::Error(_) = result_left {
                        return (result_left, env);
                    }
                    let result_right: Object;
                    (result_right, env) = evaluate(ast::Node::Expression(*right.unwrap()), env);
                    if let Object::Error(_) = result_right {
                        return (result_right, env);
                    }
                    (
                        evaluate_infix_expression(operator, result_left, result_right),
                        env,
                    )
                }
                ast::Expression::IfExpression {
                    token: _,
                    condition,
                    consequence,
                    alternative,
                } => {
                    let result_condition: Object;
                    (result_condition, env) = evaluate(ast::Node::Expression(*condition), env);
                    if let Object::Error(_) = result_condition {
                        return (result_condition, env);
                    }
                    if is_truthy(result_condition) {
                        evaluate(ast::Node::BlockStatement(consequence), env)
                    } else if alternative.is_some() {
                        evaluate(ast::Node::BlockStatement(alternative.unwrap()), env)
                    } else {
                        (NULL_OBJ, env)
                    }
                }
                ast::Expression::FunctionLiteral {
                    token,
                    parameters,
                    body,
                } => (
                    Object::Function(Function {
                        parameters,
                        body,
                        env: env.clone(),
                    }),
                    env,
                ),
                // expr => evaluate(ast::Node::Expression(expr)),
                ast::Expression::CallExpression {
                    token,
                    function,
                    arguments,
                } => {
                    println!("CallExpression. function: {}", function.to_string());
                    println!("CallExpression. arguments before: {:?}", arguments);
                    let (function, env) = evaluate(ast::Node::Expression(*function), env);
                    if let Object::Error(_) = function {
                        return (function, env);
                    }
                    println!("CallExpression. function after: {:?}", function);
                    let (arguments, env) = evaluate_expressions(arguments, env);
                    if arguments.len() == 1 {
                        if let Object::Error(_) = *arguments[0] {
                            return (*arguments[0].clone(), env);
                        }
                    }
                    // let value = evaluate_prefix_expression(function, right);
                    // println!("CallExpression RESULT: value: {:?}", value);
                    apply_function(function, arguments, env)
                }
            },
            // _stmt => panic!("Not implemented {}", _stmt.to_string(z)),
        },
        ast::Node::Expression(expr) => match expr {
            ast::Expression::IntegerLiteral { token: _, value } => (Object::Integer(value), env),
            ast::Expression::Boolean { token: _, value } => (Object::Boolean(value), env),
            ast::Expression::Identifier { token, value } => {
                let id: Object;
                (id, env) = eval_identifier(value, env);
                (id, env)
            }
            ast::Expression::PrefixExpression {
                token: _,
                operator,
                right,
            } => {
                println!("PrefixExpression. Operator: {}", operator);
                println!("PrefixExpression. right before: {:?}", right);
                let (right, env) = evaluate(ast::Node::Expression(*right.unwrap()), env);
                if let Object::Error(_) = right {
                    return (right, env);
                }
                println!("PrefixExpression. right after: {:?}", right);
                let value = evaluate_prefix_expression(operator, right);
                println!("PrefixExpression RESULT: value: {:?}", value);
                (value, env)
            }
            ast::Expression::InfixExpression {
                token: _,
                left,
                operator,
                right,
            } => {
                println!("PrefixExpression. Operator: {}", operator);
                let (left, env) = evaluate(ast::Node::Expression(*left.unwrap()), env);
                if let Object::Error(_) = left {
                    return (left, env);
                }
                let (right, env) = evaluate(ast::Node::Expression(*right.unwrap()), env);
                if let Object::Error(_) = right {
                    return (right, env);
                }
                println!("PrefixExpression. right: {:?}", right);
                let value = evaluate_infix_expression(operator, left, right);
                println!("PrefixExpression RESULT: value: {:?}", value);
                (value, env)
            }
            ast::Expression::CallExpression {
                token,
                function,
                arguments,
            } => {
                println!("CallExpression. function: {}", function.to_string());
                println!("CallExpression. arguments before: {:?}", arguments);
                let (function, env) = evaluate(ast::Node::Expression(*function), env);
                if let Object::Error(_) = function {
                    return (function, env);
                }
                println!("CallExpression. function after: {:?}", function);
                let (arguments, env) = evaluate_expressions(arguments, env);
                if arguments.len() == 1 {
                    if let Object::Error(_) = *arguments[0] {
                        return (*arguments[0].clone(), env);
                    }
                }
                // let value = evaluate_prefix_expression(function, right);
                // println!("CallExpression RESULT: value: {:?}", value);
                apply_function(function, arguments, env)
            }
            ast::Expression::FunctionLiteral {
                token,
                parameters,
                body,
            } => (
                Object::Function(Function {
                    parameters,
                    body,
                    env: env.clone(),
                }),
                env,
            ),
            ast::Expression::IfExpression {
                token: _,
                condition,
                consequence,
                alternative,
            } => {
                let result_condition: Object;
                (result_condition, env) = evaluate(ast::Node::Expression(*condition), env);
                if let Object::Error(_) = result_condition {
                    return (result_condition, env);
                }
                if is_truthy(result_condition) {
                    evaluate(ast::Node::BlockStatement(consequence), env)
                } else if alternative.is_some() {
                    evaluate(ast::Node::BlockStatement(alternative.unwrap()), env)
                } else {
                    (NULL_OBJ, env)
                }
            }
        },
        ast::Node::BlockStatement(block_statement) => {
            eval_statements(block_statement.statements, env)
        }
        ast::Node::Program(program) => eval_statements(program.statements, env),
    }
}

fn apply_function(
    function: Object,
    arguments: Vec<Box<Object>>,
    env: Environment,
) -> (Object, Environment) {
    match function {
        Object::Function(func) => {
            let extended_env = extend_function_env(Object::Function(func.clone()), arguments, env);
            println!("apply_function. extended_env: {:?}", extended_env);
            let (evaluated, env) = eval_statements(func.body.statements, extended_env);

            (unwrap_result_object(evaluated), env)
        }
        _ => (Object::Error("not a function".to_string()), env),
    }
}
fn unwrap_result_object(obj: Object) -> Object {
    if let Object::ReturnValue(value) = obj {
        *value
    } else {
        obj
    }
}

fn extend_function_env(
    function: Object,
    arguments: Vec<Box<Object>>,
    env: Environment,
) -> Environment {
    let mut env = Environment::new_enclosed_env(Some(env));
    match function {
        Object::Function(func) => {
            for (i, param) in func.parameters.iter().enumerate() {
                println!("extend_function_env. param: {}", param.to_string());
                env.set(param.to_string(), *arguments[i].clone());
            }
        }
        _ => panic!("Not a function"),
    }
    env
}
//     // for (i, param) in function.parameters.iter().enumerate() {
//     //     env.set(param.value.clone(), arguments[i].clone());
//     // }
//     // todo!();
// }

fn evaluate_expressions(
    exps: Vec<Box<ast::Expression>>,
    env: Environment,
) -> (Vec<Box<Object>>, Environment) {
    let mut objs: Vec<Box<Object>> = vec![];
    for exp in exps {
        let (exp, env) = evaluate(ast::Node::Expression(*exp), env.clone());
        if let Object::Error(_) = exp {
            return (vec![Box::new(exp)], env);
        }
        objs.push(Box::new(exp));
    }
    (objs, env)
}

fn evaluate_prefix_expression(operator: String, right: Object) -> Object {
    match operator.as_str() {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_operator_expression(&operator, right),
        "(" => right,
        _ => NULL_OBJ,
    }
}
fn eval_identifier(ident: String, env: Environment) -> (Object, Environment) {
    match env.get(&ident) {
        Some(obj) => (obj.clone(), env),
        None => {
            let err = IdentifierNotFound::new(ident.to_string());
            return (Object::Error(err.to_string()), env);
        }
    }
}

fn is_truthy(obj: Object) -> bool {
    match obj {
        Object::Boolean(b) => b,
        Object::Null => false,
        _ => true,
    }
}

fn eval_statements(statements: Vec<ast::Statement>, mut env: Environment) -> (Object, Environment) {
    let mut result = NULL_OBJ;
    for stmt in statements.iter() {
        (result, env) = evaluate(ast::Node::Statement(stmt.clone()), env);
        if let Object::Error(_) = result {
            return (result, env);
        }
        if let Object::ReturnValue(_) = result {
            return (result, env);
        }
    }
    (result, env)
}

fn expect_type(obj: Object, operator: &str, expected_type: &str) -> Object {
    println!("expect_type. obj: {:?}", obj);
    if obj.obj_type() == expected_type {
        obj
    } else {
        let err = TypeMismatch::new(obj, operator.to_string(), Some(INTEGER_STR.to_string()));
        return Object::Error(err.to_string());
    }
}

fn evaluate_infix_expression(operator: String, left: Object, right: Object) -> Object {
    println!(
        "evaluate_infix_expression. left: {:?}, right: {:?}",
        left, right
    );
    match left.obj_type().as_str() {
        INTEGER_STR => {
            let left_check = expect_type(left.clone(), &operator, INTEGER_STR);
            if let Object::Error(err) = left_check {
                return Object::Error(err);
            }
            let right_check = expect_type(right.clone(), &operator, INTEGER_STR);
            if let Object::Error(err) = right_check {
                return Object::Error(err);
            }
            let left = left.inspect().parse::<i64>().unwrap();
            let right = right.inspect().parse::<i64>().unwrap();
            return eval_integer_infix_expression(operator, left, right);
        }
        BOOL_STR => {
            let left = left.inspect().parse::<bool>().unwrap();
            let right = right.inspect().parse::<bool>().unwrap();
            return eval_boolean_infix_expression(operator, left, right);
        }
        _ => NULL_OBJ,
    }
}

fn eval_integer_infix_expression(operator: String, left: i64, right: i64) -> Object {
    match operator.as_str() {
        "+" => Object::Integer(left + right),
        "-" => Object::Integer(left - right),
        "*" => Object::Integer(left * right),
        "/" => Object::Integer(left / right),
        "<" => Object::Boolean(left < right),
        ">" => Object::Boolean(left > right),
        "==" => Object::Boolean(left == right),
        "!=" => Object::Boolean(left != right),
        _ => NULL_OBJ,
    }
}

fn eval_boolean_infix_expression(operator: String, left: bool, right: bool) -> Object {
    match operator.as_str() {
        "==" => Object::Boolean(left == right),
        "!=" => Object::Boolean(left != right),
        _ => {
            let err =
                UnknownOperator::new(BOOL_STR.to_string(), operator, Some(BOOL_STR.to_string()));
            Object::Error(err.to_string())
        }
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    println!("evalBang!");
    println!("right: {:?}", right);
    match right {
        Object::Boolean(b) => match b {
            true => FALSE_OBJ,
            false => TRUE_OBJ,
        },
        Object::Null => TRUE_OBJ,
        _ => TRUE_OBJ,
    }
}
fn eval_minus_operator_expression(operator: &str, right: Object) -> Object {
    if right.obj_type() != INTEGER_STR {
        let err = TypeMismatch::new(right, operator.to_string(), None);
        return Object::Error(err.to_string());
    }
    match right {
        Object::Integer(i) => Object::Integer(-i),
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast;
    use crate::lexer;
    use crate::parser;

    fn get_eval(input: String) -> Object {
        let lexer = lexer::Lexer::new(input);
        let mut parser = parser::Parser::new(lexer);
        let program = parser.parse_program();
        let env = Environment::new(None);
        let (obj, env) = evaluate(ast::Node::Program(program), env);
        obj
        // let mut ret = None;
        // for stmt in &program.statements {
        //     println!("stmt: {}", stmt.to_string());
        // ret = Some(evaluate(ast::Node::Statement(stmt.clone())));
        // println!("RETURN: {:?}", ret);
        // }
        // ret.unwrap()
    }
    #[test]
    // #[ignore]
    fn test_integer_expression() {
        let inputs = [
            "5",
            "10",
            "-5",
            "-10",
            "5 + 5 + 5 + 5 - 10",
            "2 * 2 * 2 * 2 * 2",
            "-50 + 100 + -50",
            "5 * 2 + 10",
            "5 + 2 * 10",
            "20 + 2 * -10",
            "50 / 2 * 2 + 10",
            "2 * (5 + 10)",
            "3 * 3 * 3 + 10",
            "3 * (3 * 3) + 10",
            "(5 + 10 * 2 + 15 / 3) * 2 + -10",
        ];
        let expected = [5, 10, -5, -10, 10, 32, 0, 20, 25, 0, 60, 30, 37, 37, 50];
        for (i, input) in inputs.iter().enumerate() {
            let actual = get_eval(input.to_string());
            assert_eq!(actual.obj_type(), INTEGER_STR);
            assert_eq!(actual.inspect(), expected[i].to_string());
            // assert_eq!((*actual as Integer).value, 5);
        }
    }
    #[test]
    // #[ignore]
    fn test_boolean_expression() {
        let inputs = [
            "true",
            "false",
            "1 < 2",
            "1 > 2",
            "1 < 1",
            "1 > 1",
            "1 == 1",
            "1 != 1",
            "1 == 2",
            "1 != 2",
            "true == true",
            "false == false",
            "true == false",
            "true != false",
            "false != true",
            "(1 < 2) == true",
            "(1 < 2) == false",
            "(1 > 2) == true",
            "(1 > 2) == false",
        ];
        let expected = [
            true, false, true, false, false, false, true, false, false, true, true, true, false,
            true, true, true, false, false, true,
        ];
        for (i, input) in inputs.iter().enumerate() {
            let actual = get_eval(input.to_string());
            assert_eq!(actual.obj_type(), BOOL_STR);
            assert_eq!(actual.inspect(), expected[i].to_string());
            // assert_eq!((*actual as Integer).value, 5);
        }
    }
    #[test]
    // #[ignore]
    fn test_bang_operator() {
        let inputs = ["!true", "!false"];
        let expected = [false, true];
        for (i, input) in inputs.iter().enumerate() {
            let actual = get_eval(input.to_string());
            assert_eq!(actual.obj_type(), BOOL_STR);
            assert_eq!(actual.inspect(), expected[i].to_string());
            // assert_eq!((*actual as Integer).value, 5);
        }
    }
    #[test]
    // #[ignore]
    fn test_if_expression() {
        let inputs = [
            "if (true) { 10 }",
            "if (false) { 10 }",
            "if (1) { 10 }",
            "if (1 < 2) { 10 }",
            "if (1 > 2) { 10 }",
            "if (1 > 2) { 10 } else { 20 }",
            "if (1 < 2) { 10 } else { 20 }",
        ];
        let expected = [
            Object::Integer(10),
            NULL_OBJ,
            Object::Integer(10),
            Object::Integer(10),
            NULL_OBJ,
            Object::Integer(20),
            Object::Integer(10),
        ];
        for (i, input) in inputs.iter().enumerate() {
            let actual = get_eval(input.to_string());
            let actual = actual;
            assert_eq!(actual, expected[i]);
        }
    }
    #[test]
    // #[ignore]
    fn test_return_statements() {
        let inputs = [
            "return 10;",
            "return 10; 9;",
            "return 2 * 5; 9;",
            "9; return 2 * 5; 9;",
            "if (10 > 1) { if (10 > 1) { return 10; } return 1; }",
        ];
        let expected = [
            Object::ReturnValue(Box::new(Object::Integer(10))),
            Object::ReturnValue(Box::new(Object::Integer(10))),
            Object::ReturnValue(Box::new(Object::Integer(10))),
            Object::ReturnValue(Box::new(Object::Integer(10))),
            Object::ReturnValue(Box::new(Object::Integer(10))),
        ];
        for (i, input) in inputs.iter().enumerate() {
            let actual = get_eval(input.to_string());
            let actual = actual;
            assert_eq!(actual, expected[i]);
        }
    }
    #[test]
    // #[ignore]
    fn test_let_statements() {
        let inputs = [
            "let a = 5; a;",
            "let a = 5 * 5; a;",
            "let a = 5; let b = a; b;",
            "let a = 5; let b = a; let c = a + b + 5; c;",
        ];
        let expected = [
            Object::Integer(5),
            Object::Integer(25),
            Object::Integer(5),
            Object::Integer(15),
        ];
        for (i, input) in inputs.iter().enumerate() {
            let actual = get_eval(input.to_string());
            let actual = actual;
            assert_eq!(actual, expected[i]);
        }
    }
    #[test]
    // #[ignore]
    fn test_function_object() {
        let inputs = ["fn(x) { x + 2; };"];
        for (i, input) in inputs.iter().enumerate() {
            let actual = get_eval(input.to_string());
            // let actual = actual;
            println!("actual {}", actual.inspect());
            match actual {
                Object::Function(func) => {
                    println!("parameters {:?}", func.parameters);
                    assert_eq!(func.parameters.len(), 1);
                    assert_eq!(func.parameters[0].to_string(), "x");
                    assert_eq!(func.body.to_string(), "(x + 2)");
                }
                _ => panic!("not a function"),
            }
        }
        // assert_eq!(1, 0);
    }
    #[test]
    // #[ignore]
    fn test_function_application() {
        let inputs = [
            "let identity = fn(x) { x; }; identity(5);",
            "let double = fn(x) { x * 2; }; double(5);",
            "let add = fn(x, y) { x + y; }; add(5, 5);",
            "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
            "let identity = fn(x) { return x; }; identity(5);",
            "fn(x) { x; }(5)",
        ];
        let expected = [
            Object::Integer(5),
            Object::Integer(10),
            Object::Integer(10),
            Object::Integer(20),
            Object::Integer(5),
            Object::Integer(5),
        ];
        for (i, input) in inputs.iter().enumerate() {
            let actual = get_eval(input.to_string());
            // let actual = actual;
            println!("actual {}", actual.inspect());
            assert_eq!(actual, expected[i]);
        }
        // assert_eq!(1, 0);
    }
    #[test]
    // #[ignore]
    fn test_error_handling() {
        let inputs = [
            "5 + true;",
            "5 + true; 5;",
            "-true",
            "true + false;",
            "5; true + false; 5",
            "if (10 > 1) { true + false; }",
            "foobar",
        ];
        let expected = [
            "ERROR: type mismatch: INTEGER + BOOLEAN",
            "ERROR: type mismatch: INTEGER + BOOLEAN",
            "ERROR: type mismatch: - BOOLEAN",
            "ERROR: unknown operator: BOOLEAN + BOOLEAN",
            "ERROR: unknown operator: BOOLEAN + BOOLEAN",
            "ERROR: unknown operator: BOOLEAN + BOOLEAN",
            "ERROR: identifier not found: foobar",
        ];
        for (i, input) in inputs.iter().enumerate() {
            let actual = get_eval(input.to_string());
            let actual = actual;
            assert_eq!(actual.inspect(), expected[i]);
        }
    }
}
