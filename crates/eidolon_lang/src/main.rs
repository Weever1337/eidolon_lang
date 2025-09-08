use eidolon_lang::interpreter::evaluate;
use eidolon_lang::parse_eidolon_source;
use std::collections::HashMap;
use std::fs;

fn main() {
    // todo: do better tests
    let file_path = "examples/test_math.eidolon";
    let source_code = match fs::read_to_string(file_path) {
        Ok(code) => code,
        Err(e) => {
            eprintln!("Error reading file {}: {}", file_path, e);
            return;
        }
    };

    match parse_eidolon_source(&source_code) {
        Ok(ast) => {
            match evaluate(&ast, &HashMap::new()) {
                Ok(result) => {
                    println!("✅ Eidolon execution finished successfully!");
                    println!("> Result: {}", result);
                }
                Err(e) => {
                    eprintln!("❌ Eidolon runtime error: {}", e.message);
                }
            }
        }
        Err(e) => {
            eprintln!("{}", e);
        }
    }
}
