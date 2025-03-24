#!/usr/bin/perl
use strict;
use warnings;
use v5.10;

sub precedence{
    my ($oprtr) = @_;
    return 2 if $oprtr =~ /[*\/]/;
    return 1 if $oprtr =~ /[+-]/;
    return 0;
}

my %operations = (
    '+' => sub { $_[0] + $_[1] },
    '-' => sub { $_[0] - $_[1] },
    '*' => sub { $_[0] * $_[1] },
    '/' => sub {
        return (0, "Division by zero") if $_[1] == 0;
        int($_[0] / $_[1]);
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
    return $expr =~ /^[\d\+\-\*\/\(\)\s]+$/;
}

# Convert to postfix algorithm (called Shunting-yard )
sub convert {
    my ($expr) = @_;
    my @output;
    my @operators;
    my $num = '';
    
    for my $c (split //, $expr){
        if ($c =~ /\d/){        # if digit, add to num
            $num .= $c;
        } 
        elsif ($c eq ' '){
            if ($num ne ''){
                push @output, $num;
                $num = '';
            }
        }
        else{
            if ($num ne ''){
                push @output, $num;
                $num = '';
            }
            
            if ($c eq '('){
                push @operators, $c;
            }
            elsif ($c eq ')'){
                while (@operators && $operators[-1] ne '(') {
                    push @output, pop @operators;
                }
                pop @operators if @operators; # Remove '('
            }
            else{
                while (@operators &&                   # evaluate higher precedence operator
                       precedence($operators[-1]) >= precedence($c) &&
                       $operators[-1] ne '(') {
                    push @output, pop @operators;
                }
                push @operators, $c;
            }
        }
    }
    
    push @output, $num if $num ne '';
    while (@operators) {
        my $oprtr = pop @operators;
        push @output, $oprtr unless $oprtr =~ /[()]/;
    }
    
    return @output;
}

# Evaluate postfix expression
sub evaluate{
    my (@postfix) = @_;
    my @stack;
    
    for my $token (@postfix){
        if ($token =~ /^\d+$/){
            push @stack, $token;
        }
        else{
            return (0, "Not enough operands") if @stack < 2;
            my $b = pop @stack;
            my $a = pop @stack;
            my ($result, $error) = apply_operator($a, $b, $token);
            return (0, $error) if defined $error;
            push @stack, $result;
        }
    }
    
    return (0, "Empty expression") unless @stack;
    return (0, "Too many operands") if @stack > 1;
    return (pop @stack, undef);
}

say "Enter an arithmetic expression:";
while (1) {
    print "> ";
    my $expr = <STDIN>;
    last unless defined $expr;
    
    $expr =~ s/^\s+|\s+$//g;
    last if $expr eq '';
    
    unless (is_valid($expr)) {
        say "Invalid characters in expression";
        next;
    }
    
    my @postfix = convert($expr);
    my ($result, $error) = evaluate(@postfix);
    
    if (defined $error) {
        say "Error: $error";
    } else {
        say "Result: $result";
    }
}

say "Exiting calculator";
