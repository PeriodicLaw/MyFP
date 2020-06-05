mod lexer;
use std::io;
use linefeed::{Interface, ReadResult};

fn main() -> io::Result<()> {
    let interface = Interface::new("MyFP")?;
    interface.set_prompt("> ")?;
    
    let mut input: String = String::new();
    
    while let ReadResult::Input(line) = interface.read_line()? {
        if !line.trim().is_empty() {
            interface.add_history_unique(line.clone());
        }
        
        if line.trim().ends_with(';') {
            input.push_str(&line[..]);
            let token_stream = lexer::TokenStream::new(&input);
            for token in token_stream{
                print!("{:?}, ", token);
            }
            println!();
            input.clear();
            interface.set_prompt("> ")?;
        } else {
            input.push_str(&line[..]);
            interface.set_prompt("  ")?;
        }
        
    }
    
    Ok(())
}
