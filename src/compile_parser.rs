#[macro_export]
macro_rules! lolcode_inner {
    (stmt; GTFO) => {{ AST::Gtfo }};
    (stmt; FOUND YR $($code:tt) +) => {{
        AST::FoundYr(lolcode_inner!(expr; $($code) +))
    }};
    (stmt; I HAS A $ident:ident ITZ $($code:tt) +) => {{
        AST::IHasA(lolcode_inner!(ident; $ident), lolcode_inner!(expr; $($code) +))
    }};
    (stmt; I HAS A $ident:ident) => {{
        AST::IHasA(lolcode_inner!(ident; $ident), Value::Noob)
    }};
    (stmt; $ident:ident R $($code:tt) +) => {{
        AST::R(lolcode_inner!(ident; $ident), lolcode_inner!(expr; $($code) +))
    }};
    (stmt; O RLY?
            $(YA RLY ($($yarly:tt) +))*
            $(MEBBE ($($mebbe_cond:tt) +), ($($mebbe_block:tt) +))*
            $(NO WAI ($($nowai:tt) +))*) => {{
        let mut mebbe = Vec::new();
        $(
            mebbe.push((lolcode_inner!(expr; $($mebbe_cond) +), lolcode!(($($mebbe_block) +))));
        )+
        AST::ORly(
            lolcode!($(($($yarly) +))*),
            mebbe,
            lolcode!($(($($nowai) +))*)
        )
    }};
    (stmt; $($code:tt) +) => {{
        AST::It(lolcode_inner!(expr; $($code) +))
    }};

    (expr; IT) => {{ Expr::It }};
    (expr; SUM OF ($($code1:tt) +) AN ($($code2:tt) +)) => {{
        lolcode_inner!(x_of; SumOf, ($($code1) +), ($($code2) +))
    }};
    (expr; BOTH SAEM ($($code1:tt) +) AN ($($code2:tt) +)) => {{
        lolcode_inner!(x_of; BothSaem, ($($code1) +), ($($code2) +))
    }};
    (expr; int $value:expr) => {{ Expr::Value(Value::Numbr($value)) }};
    (expr; $ident:ident) => {{ Expr::Var(lolcode_inner!(ident; $ident)) }};
    (expr; $value:expr) => {{ Expr::Value(Value::Yarn(String::from($value))) }};

    (x_of; $type:ident, ($($code1:tt) +), ($($code2:tt) +)) => {{
        Expr::$type(
            Box::new(lolcode_inner!(expr; $($code1) +)),
            Box::new(lolcode_inner!(expr; $($code2) +))
        )
    }};
    (ident; $ident:ident) => {{ String::from(stringify!($ident)) }};
}
#[macro_export]
macro_rules! lolcode {
    () => {{ Vec::new() }};
    ($(($($code:tt) +))*) => {{
        use parser::*;
        use types::Value;

        let mut asts = Vec::new();
        $(
            asts.push(lolcode_inner!(stmt; $($code) +));
        )*
        asts
    }}
}

#[cfg(test)]
mod tests {
    use parser::*;
    use types::Value;

    #[test]
    fn i_has_a() {
        assert_eq!(
            lolcode!(
                (I HAS A COW ITZ "Hello World")
                (COW R SUM OF (int 1) AN (int 2))
                (FOUND YR COW)
            ),
            &[
                AST::IHasA("COW".to_string(), Expr::Value(Value::Yarn("Hello World".to_string()))),
                AST::R("COW".to_string(), Expr::SumOf(
                    Box::new(Expr::Value(Value::Numbr(1))),
                    Box::new(Expr::Value(Value::Numbr(2)))
                )),
                AST::FoundYr(Expr::Var("COW".to_string()))
            ]
        );
    }

    #[test]
    fn orly() {
        assert_eq!(
            lolcode!(
                (BOTH SAEM (int 1) AN (int 1))
                (
                    O RLY?
                        YA RLY (
                            FOUND YR int 0
                        )
                        MEBBE (BOTH SAEM (int 1) AN (int 2)), (
                            FOUND YR int 1
                        )
                        NO WAI (
                            FOUND YR int 2
                        )
                )
            ),
            &[
                AST::It(Expr::BothSaem(
                    Box::new(Expr::Value(Value::Numbr(1))),
                    Box::new(Expr::Value(Value::Numbr(1)))
                )),
                AST::ORly(
                    vec![AST::FoundYr(Expr::Value(Value::Numbr(0)))],
                    vec![(
                        Expr::BothSaem(
                            Box::new(Expr::Value(Value::Numbr(1))),
                            Box::new(Expr::Value(Value::Numbr(2)))
                        ),
                        vec![AST::FoundYr(Expr::Value(Value::Numbr(1)))]
                    )],
                    vec![AST::FoundYr(Expr::Value(Value::Numbr(2)))]
                )
            ]
        );
        assert_eq!(
            lolcode!(
                (BOTH SAEM (int 1) AN (int 1))
                (
                    O RLY?
                        MEBBE (BOTH SAEM (int 1) AN (int 2)), (
                            FOUND YR int 0
                        )
                        MEBBE (BOTH SAEM (int 1) AN (int 3)), (
                            FOUND YR int 1
                        )
                )
            ),
            &[
                AST::It(Expr::BothSaem(
                    Box::new(Expr::Value(Value::Numbr(1))),
                    Box::new(Expr::Value(Value::Numbr(1)))
                )),
                AST::ORly(
                    vec![],
                    vec![
                        (
                            Expr::BothSaem(
                                Box::new(Expr::Value(Value::Numbr(1))),
                                Box::new(Expr::Value(Value::Numbr(2)))
                            ),
                            vec![AST::FoundYr(Expr::Value(Value::Numbr(0)))]
                        ),
                        (
                            Expr::BothSaem(
                                Box::new(Expr::Value(Value::Numbr(1))),
                                Box::new(Expr::Value(Value::Numbr(3)))
                            ),
                            vec![AST::FoundYr(Expr::Value(Value::Numbr(1)))]
                        ),
                    ],
                    vec![]
                )
            ]
        );
    }
}
