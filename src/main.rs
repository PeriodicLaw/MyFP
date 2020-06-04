mod lexer;
use std::io;
use linefeed::{Interface, ReadResult};

fn main() -> io::Result<()> {
    let interface = Interface::new("MyFP")?;
    interface.set_prompt("> ")?;
    
    while let ReadResult::Input(input) = interface.read_line()? {
        let token_stream = lexer::TokenStream::new(&input);
        for token in token_stream{
            print!("{:?}, ", token);
        }
        println!();
    }
    
    Ok(())
}
