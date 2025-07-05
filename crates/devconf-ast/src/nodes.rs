use devconf_nodes::ast::AstExpr;

#[derive(Debug, Clone, PartialEq)]
pub struct AstScope(pub Vec<AstStatement>);

#[derive(Debug, Clone, PartialEq)]
pub enum PathSegment {
    Static(String),
    Dynamic(Box<AstExpr>),
}

#[derive(Debug, Clone)]
pub struct MacroDefinition {
    pub params: Vec<String>,
    pub defaults: Vec<(String, AstExpr)>,
    pub body: AstScope,
}

impl From<AstStatement> for MacroDefinition {
    fn from(val: AstStatement) -> Self {
        if let AstStatement::TemplateDefinition {
            params,
            defaults,
            body,
            ..
        } = val
        {
            MacroDefinition {
                params,
                defaults,
                body,
            }
        } else {
            panic!("Unvalid cast.")
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AstStatement {
    Comment,
    Expression(Box<AstExpr>),
    Conditional {
        test: Box<AstExpr>,
        body: AstScope,
        otherwise: Option<AstScope>,
    },
    Assignation {
        path: Vec<PathSegment>,
        value: Box<AstExpr>,
    },
    TemplateDefinition {
        name: String,
        params: Vec<String>,
        defaults: Vec<(String, AstExpr)>,
        body: AstScope,
    },
    TemplateCalling {
        name: String,
        args: Vec<AstExpr>,
    },
}
