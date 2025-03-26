use std::collections::{VecDeque, HashMap};
use std::io;

fn precedence(oprtr: char) -> i32{
    match oprtr{
        '+' | '-' =>1,
        '*' | '/' =>2,
        _ => 0,
    }
}

fn apply_operator(a: i32, b: i32, oprtr: char) -> Result<i32, String>{
    match oprtr{
        '+' => Ok(a+b),
        '-' => Ok(a-b),
        '*' => Ok(a*b),
        '/' => {
                if b==0 {
                    Err("Division by zero".to_string())
                } 
                else{
                    Ok(a/b)
                }
        }
        _ => Err("Unknown operator".to_string()),
    }
}

fn is_valid(expr: &str) -> bool{
    expr.chars().all(|ch| 
        ch.is_ascii_alphanumeric() || "+-*/() _ ".contains(ch)
    )
}

fn is_valid_variable_name(name: &str) -> bool{
    let mut chars = name.chars();
    let first_char = chars.next();

    if let Some(c) = first_char{
        if !(c.is_ascii_alphabetic() || c == '_'){ return false;}
    } 
    else{ return false;}

    chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
}

fn convert(expr: &str) -> Vec<String>{
    let mut output = Vec::new();
    let mut operators = VecDeque::new();
    let mut num = String::new();
    let mut curr_var = String::new();

    for ch in expr.chars(){
        
        if ch.is_ascii_digit(){

            if !curr_var.is_empty(){
                output.push(curr_var.clone());
                curr_var.clear();
            }
            num.push(ch);
        } 
        else if ch.is_ascii_alphabetic() || ch == '_'{
            if !num.is_empty(){
                output.push(num.clone());
                num.clear();
            }
            curr_var.push(ch);
        } 
        else{
            if !num.is_empty(){
                output.push(num.clone());
                num.clear();
            }

            if !curr_var.is_empty(){
                output.push(curr_var.clone());
                curr_var.clear();
            }
            
            if ch == ' '{ continue;} 
            else if ch == '('{ operators.push_back(ch);} 
            else if ch == ')'{
                
                let mut is_paren=false;
                
                while let Some(oprtr) = operators.pop_back(){
                    
                    if oprtr == '('{
                        is_paren=true;
                        break;
                    }
                    output.push(oprtr.to_string());
                }
                
                if !is_paren {}
            } 
            else if "+-*/".contains(ch){
                
                while let Some(&top_op) = operators.back(){
                    
                    if top_op != '(' && precedence(top_op) >= precedence(ch){
                        output.push(operators.pop_back().unwrap().to_string());
                    } 
                    else{ break;}
                }
                operators.push_back(ch);
            }
        }
    }

    if !num.is_empty(){
        output.push(num);
    }

    if !curr_var.is_empty(){
        output.push(curr_var);
    }

    while let Some(oprtr) = operators.pop_back(){
        
        if oprtr != '(' && oprtr != ')'{
            output.push(oprtr.to_string());
        }
    }

    output
}

fn evaluate(postfix: Vec<String>, env: &HashMap<String, i32>) -> Result<i32, String>{
    let mut stack = Vec::new();
    
    for token in postfix{

        if let Ok(num) = token.parse::<i32>(){
            stack.push(num);
            continue;
        }

        if token.len() == 1{
            let ch = token.chars().next().unwrap();
            
            if "+-*/".contains(ch){
                
                if stack.len() < 2{
                    return Err("Not enough operands".to_string());
                }

                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();
                let result = apply_operator(a, b, ch)?;
                stack.push(result);
                continue;
            }
        }

        match env.get(&token) {
            Some(value) => stack.push(*value),
            None => return Err(format!("Undefined variable '{}'", token)),
        }
    }

    if stack.is_empty() { Err("Empty expression".to_string())} 
    else if stack.len() > 1 { Err("Too many operands".to_string())} 
    else { Ok(stack.pop().unwrap())}
}

fn main() {
    let mut environment = HashMap::new();
    
    loop{
        let mut expr = String::new();
        println!("> ");
        
        match io::stdin().read_line(&mut expr){
            Ok(_) =>{
                let expr = expr.trim();
                
                if expr.is_empty(){
                    println!("Exiting calculator");
                    break;
                }
                
                if let Some(eq_pos) = expr.find('='){
                    let (var_part, expr_part) = expr.split_at(eq_pos);
                    let var_name = var_part.trim();
                    let expr = expr_part[1..].trim();

                    if !is_valid_variable_name(var_name){
                        println!("Invalid variable name: {}", var_name);
                        continue;
                    }

                    if !is_valid(expr){
                        println!("Invalid expression");
                        continue;
                    }

                    let postfix = convert(expr);
                    match evaluate(postfix, &environment){
                        Ok(value) =>{
                            environment.insert(var_name.to_string(), value);
                            println!("{} = {}", var_name, value);
                        }
                        Err(e) => println!("Error: {}", e),
                    }
                } 
                else{

                    if !is_valid(expr){
                        println!("Invalid characters");
                        continue;
                    }

                    let postfix = convert(expr);
                    match evaluate(postfix, &environment){
                        Ok(result) => println!("Result: {}", result),
                        Err(error) => println!("Error: {}", error),
                    }
                }
            }
            Err(error) => {
                println!("Error reading input: {}", error);
                break;
            }
        }
    }
}