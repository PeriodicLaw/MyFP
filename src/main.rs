#![feature(box_patterns)]

mod ast;
mod eval;
mod lexer;
mod parser;
mod typecheck;

use linefeed::{Interface, ReadResult};
use std::io;

use crate::ast::Context;

fn main() -> io::Result<()> {
	let interface = Interface::new("MyFP")?;
	interface.set_prompt("> ")?;

	let mut input: String = String::new();

	let mut ct = Context::new();

	while let ReadResult::Input(line) = interface.read_line()? {
		if !line.trim().is_empty() {
			interface.add_history_unique(line.clone());
		}

		input.push_str(&line[..]);
		if line.trim().ends_with(';') {
			let mut parser = parser::Parser::new(&input);
			// 语法分析
			match parser.parse() {
				Ok(mut ast) => {
					// println!("{:?}", ast);
					// 类型检查
					if let Ok(ty) = ast.typecheck(&mut ct) {
						// 化简求值
						if let Ok(expr) = ast.expr.eval(&ct) {
							// println!("before:");
							// println!("expr = {}", expr);
							// ast.tyct.eprint_bound();

							ast.tyct.rename();
							let ty = ty.simpl(&ast.tyct);
							let expr = expr.simpl(&ast.tyct);
							ast.tyct.flush_bounds();

							// println!("after:");
							// println!("expr = {}", expr);

							match ast.id {
								Some(id) => {
									// let表达式，需要为其添加上下文
									println!("{} : {}{}\n{} = {}", id, ast.tyct, ty, id, expr);
									ct.add(id, ast.tyct.free, ty, expr);
								}
								None => println!("{} : {}{}", expr, ast.tyct, ty), // 匿名表达式，直接给出结果即可
							}
						}
					}
				}
				Err(pos) => {
					if let Some(pos) = pos {
						let pos0 = if pos < 5 { pos } else { 5 };
						let near: String = input.chars().skip(pos - pos0).take(pos0 + 10).collect();
						eprintln!("error occurs at: {}", near);
						eprintln!("                 {}^", " ".repeat(pos0));
					} else {
						eprintln!("error occurs at the end of input stream.");
					}
				}
			}
			ct.pop_all(); // 由于类型检查中可能出现错误，需要手动把此时出现的局部上下文清空
			println!("");
			input.clear();
			interface.set_prompt("> ")?;
		} else {
			input.push(' '); // 对于换行，我们手动给两行之间加上空格
			interface.set_prompt("  ")?;
		}
	}

	Ok(())
}
