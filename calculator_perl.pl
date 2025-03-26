#!/usr/bin/perl
use strict;
use warnings;
use v5.10;

my %env;  # environment var 

sub precedence{
    my ($oprtr) = @_;
    return 2 if $oprtr =~/[*\/]/;
    return 1 if $oprtr =~/[+-]/;
    return 0;
}

my %operations = (
    '+' => sub {$_[0]+$_[1]},
    '-' => sub {$_[0]-$_[1]},
    '*' => sub {$_[0]*$_[1]},
    '/' => sub {
        return (0, "Division by zero") if $_[1]==0;
        int($_[0]/$_[1]);
    }
);

sub apply_operator{
    my ($a, $b, $oprtr) = @_;
    
    if (exists $operations{$oprtr}){
        my $result = eval { $operations{$oprtr}->($a, $b) };
        return $@ ? (0, $@) : ($result, undef);
    }
    return (0, "Unknown operator: $oprtr");
}

sub is_valid{
    my ($expr) = @_;
    return $expr =~ /^[a-zA-Z0-9_+\-*\/()\s]+$/;
}
sub is_valid_variable_name{
    my ($name) = @_;
    return $name =~ /^[a-zA-Z_][a-zA-Z0-9_]*$/;
}

# the shunting yard algorithm
sub convert{
    my ($expr) = @_;
    my @output;
    my @operators;
    my ($num, $var) = ('', ''); 
    
    for my $c (split //, $expr){
        if ($c =~ /\d/){

            if ($var ne ''){ $var .= $c;}           # if digit in variable name 
            else{$num .= $c;}                       # normal num
        }
        elsif ($c =~ /[a-zA-Z_]/){
            if ($num ne ''){
                push @output, $num;
                $num = '';
            }
            $var .= $c;
        }
        else{
            if ($num ne ''){
                push @output, $num;
                $num = '';
            }
            if ($var ne ''){
                push @output, $var;
                $var = '';
            }
            
            next if $c eq ' ';
            
            # handling operator precedence and parentheses
            if ($c eq '('){
                push @operators, $c;
            }
            elsif ($c eq ')'){

                while (@operators && $operators[-1] ne '('){
                    push @output, pop @operators;
                }
                pop @operators if @operators;
            }
            else{

                while (@operators && precedence($operators[-1]) >= precedence($c) && $operators[-1] ne '('){
                    push @output, pop @operators;
                }
                push @operators, $c;
            }
        }
    }
    
    push @output, $num if $num ne '';
    push @output, $var if $var ne '';
    
    while (@operators){
        my $oprtr = pop @operators;
        push @output, $oprtr unless $oprtr =~ /[()]/;
    }
    
    return @output;
}

sub evaluate{
    my ($postfix_ref, $env_ref) = @_;
    my @postfix = @$postfix_ref;
    my %env = %$env_ref;
    my @stack;
    
    for my $token (@postfix){
        if ($token =~ /^\d+$/){
            push @stack, $token;
        }
        elsif (exists $operations{$token}){
            return (0, "Not enough operands") if @stack < 2;
            my $b = pop @stack;
            my $a = pop @stack;
            my ($result, $error) = apply_operator($a, $b, $token);
            return (0, $error) if defined $error;
            push @stack, $result;
        }
        else{
            # handling with variables
            if (exists $env{$token}){
                push @stack, $env{$token};
            } 
            else{return (0, "Undefined variable '$token'");}
        }
    }
    
    return (0, "Empty") unless @stack;
    return (0, "Too many operands") if @stack > 1;
    return (pop @stack, undef);
}

say "Enter an arithmetic expression (or variable assignments):";

while (1){
    print "> ";
    my $expr = <STDIN>;
    last unless defined $expr;
    
    $expr =~ s/^\s+|\s+$//g;
    last if $expr eq '';
    
    if ($expr =~ /=/){
        # variable assignment
        my ($var_name, $expr_part) = split /=/, $expr, 2;
        $var_name =~ s/^\s+|\s+$//g;
        $expr_part =~ s/^\s+|\s+$//g;
        
        if (!is_valid_variable_name($var_name)){
            say "Invalid variable name '$var_name'";
            next;
        }
        
        if (!is_valid($expr_part)){
            say "Invalid expression";
            next;
        }
        
        my @postfix = convert($expr_part);
        my ($result, $error) = evaluate(\@postfix, \%env);
        
        if (defined $error){
            say "Error: $error";
        } else{
            $env{$var_name} = $result;
            say "$var_name = $result";
        }
    } 
    else{
        # arithmetic expression
        if (!is_valid($expr)){
            say "Invalid expression";
            next;
        }
        
        my @postfix = convert($expr);
        my ($result, $error) = evaluate(\@postfix, \%env);
        
        if (defined $error){ say "Error: $error";} 
        else{say "Result: $result";}
    }
}

say "Exiting calculator";