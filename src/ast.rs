// use crate::token::Token;
//
// trait Node {
//     fn token_literal(&self) -> String;
// }
//
// trait Statement: Node {
//     fn statement_node(&self);
// }
//
// trait Expression: Node {
//     fn expression_node(&self);
// }
// struct Identifier {
//     token: Token,
//     value: String,
// }
//
// impl Node for Identifier {
//     fn token_literal(&self) -> String {
//         self.token.into()
//     }
// }
//
// impl Expression for Identifier {
//     fn expression_node(&self) {}
// }
//
// struct LetStatement {
//     token: Token,
//     name: Identifier,
//     value: dyn Expression,
// }
//
// impl Node for LetStatement {
//     fn token_literal(&self) -> String {
//         self.token.literal.clone()
//     }
// }
//
// impl Statement for LetStatement {
//     fn statement_node(&self) {}
// }
