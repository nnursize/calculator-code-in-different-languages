// RUST

use std::collections::VecDeque;
use std::io;

fn precedence(oprtr:char) -> i32{
    match oprtr{
        '+' | '-' => 1,
        '*' | '/' => 2,
        _ => 0,
    }
}

fn apply_operator(a:i32, b:i32, oprtr:char) -> Result<i32, String>{
    match oprtr{
        '+' => Ok(a + b),
        '-' => Ok(a - b),
        '*' => Ok(a * b),
        '/' => {
            if (b == 0){
                Err("Division by zero".to_string())
            } 
            else{
                Ok(a / b)
            }
        }
        _ => Err("Unknown operator".to_string()), // Warn syntax error here
    }
}

fn is_valid(expr:&str) -> bool{
    expr.chars().all(|ch| ch.is_digit(10) || "+-*/() ".contains(ch))
}

fn convert(expr:&str) -> Vec<String>{           // Convert to postfix (3+5 to 35+)
    let mut output = Vec::new();
    let mut operators = VecDeque::new();
    let mut num = String::new();
    
    for ch in expr.chars(){
        if ch.is_digit(10){
            num.push(ch);
        } 
        else if (ch == ' '){
            // Skip spaces but push any accumulated number
            if !num.is_empty(){
                output.push(num.clone());
                num.clear();
            }
        }
        else{
            if !num.is_empty(){
                output.push(num.clone());
                num.clear();
            }
            
            if (ch == '('){
                operators.push_back(ch);
            } 
            else if (ch == ')'){
                let mut is_paren = false;
                while let Some(oprtr) = operators.pop_back(){
                    if (oprtr == '('){
                        is_paren = true;
                        break;
                    }
                    output.push(oprtr.to_string());
                }
                if !is_paren{
                    // continue processing
                }
            } 
            else if ("+-*/".contains(ch)){
                while let Some(&top_op) = operators.back(){
                    if ((top_op != '(') && (precedence(top_op) >= precedence(ch))){
                        output.push(operators.pop_back().unwrap().to_string());
                    } 
                    else{
                        break;
                    }
                }
                operators.push_back(ch);
            }
        }
    }
    
    if !num.is_empty(){
        output.push(num);
    }
    
    while let Some(oprtr) = operators.pop_back(){
        if ((oprtr != '(') && (oprtr != ')')){                   // Skip unmatched parentheses
            output.push(oprtr.to_string());
        }
    }
    
    output
}

fn evaluate(postfix: Vec<String>) -> Result<i32, String>{
    let mut stack = Vec::new();
    
    for token in postfix{
        if let Ok(num) = token.parse::<i32>(){
            stack.push(num);
        } 
        else if (token.len() == 1){
            let char_token = token.chars().next().unwrap();
            
            // Need at least two operands
            if (stack.len() < 2){
                return Err("Not enough operands".to_string());
            }
            
            let b = stack.pop().unwrap();
            let a = stack.pop().unwrap();
            
            stack.push(apply_operator(a, b, char_token)?);
        }
    }
    
    if stack.is_empty() {Err("Empty expression".to_string())} 
    else if (stack.len() > 1) { Err("Too many operands".to_string())} 
    else {Ok(stack.pop().unwrap())}
}

fn main(){
    println!("Enter an arithmetic expression:");
    
    loop{
        let mut expr = String::new();
        match io::stdin().read_line(&mut expr){
            Ok(_) =>{
                let expr = expr.trim();
                
                // Exit condition
                if expr.is_empty(){
                    println!("Exiting calculator");
                    break;
                }
                
                if !is_valid(expr){
                    println!("Invalid characters");
                    continue;
                }
                
                let postfix = convert(expr);
                match evaluate(postfix){
                    Ok(result) => println!("Result: {}", result),
                    Err(error) => println!("{}", error),
                }
            },
            Err(error) =>{
                println!("Error reading input: {}", error);
                break;
            }
        }
    }
}
