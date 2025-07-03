use devconf_nodes::ast::*;

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

impl Into<MacroDefinition> for AstStatement {
    fn into(self) -> MacroDefinition {
        if let Self::TemplateDefinition {
            params,
            defaults,
            body,
            ..
        } = self
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
