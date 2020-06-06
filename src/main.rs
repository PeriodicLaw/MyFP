#![feature(box_patterns)]

mod ast;
mod lexer;
mod parser;

use linefeed::{Interface, ReadResult};
use std::io;

fn main() -> io::Result<()> {
    let interface = Interface::new("MyFP")?;
    interface.set_prompt("> ")?;

    let mut input: String = String::new();

    while let ReadResult::Input(line) = interface.read_line()? {
        if !line.trim().is_empty() {
            interface.add_history_unique(line.clone());
        }

        input.push_str(&line[..]);
        if line.trim().ends_with(';') {
            let mut parser = parser::Parser::new(&input);
            match parser.parse() {
                Ok(ast) => println!("{}", ast),
                Err(pos) => {
                    let pos = match pos {
                        Some(pos) => pos,
                        None => input.len(), // 如果token流已经结束，我们简单地把错误提示放到最末尾
                    };

                    let pos0 = if pos < 5 { pos } else { 5 };
                    let near: String = input.chars().skip(pos - pos0).take(pos0 + 10).collect();
                    eprintln!("error occurs at: {}", near);
                    eprintln!("                 {}^", " ".repeat(pos0));
                }
            }
            input.clear();
            interface.set_prompt("> ")?;
        } else {
            input.push(' '); // 对于换行，我们手动给两行之间加上空格
            interface.set_prompt("  ")?;
        }
    }

    Ok(())
}
