use pulldown_cmark as cmark;
use serde::Serialize;

use crate::error::*;
use crate::syntax_highlight::decorate_markdown;

#[derive(Debug, Clone, Serialize)]
#[serde(deny_unknown_fields)]
pub struct MarkdownBuilder {
    pub theme: Option<liquid::model::KString>,
    #[serde(skip)]
    pub syntax: std::sync::Arc<crate::SyntaxHighlight>,
}

impl MarkdownBuilder {
    pub fn build(self) -> Markdown {
        Markdown {
            theme: self.theme,
            syntax: self.syntax,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Markdown {
    theme: Option<liquid::model::KString>,
    syntax: std::sync::Arc<crate::SyntaxHighlight>,
}

use crate::error;
use cmark::Event;

pub struct DecoratedParser<'a> {
    events: std::vec::IntoIter<Event<'a>>,
    definitions: Vec<Vec<Event<'a>>>,
    current_definition: Option<std::vec::IntoIter<Event<'a>>>,
    in_sidenote: bool,
}

impl<'a> DecoratedParser<'a> {
    fn new<T: 'a>(parser: T) -> Self
    where
        T: Iterator<Item = Event<'a>>,
    {
        let mut events = parser.collect::<Vec<_>>();

        let mut definitions: Vec<Vec<Event>> = vec![];
        let mut i: usize = 0;
        while i < events.len() {
            if let Event::Start(cmark::Tag::FootnoteDefinition(tag)) = &events[i] {
                let j = i + events[i..]
                    .iter()
                    .position(|ev| match ev {
                        Event::End(cmark::TagEnd::FootnoteDefinition) => true,
                        _ => false,
                    })
                    .expect(&format!("Should find the end of footnote, {}", tag));
                definitions.push(events.drain(i..=j).collect());
            } else {
                i += 1;
            }
        }

        Self {
            events: events.into_iter(),
            definitions,
            current_definition: None,
            in_sidenote: false,
        }
    }
}

impl<'a> Iterator for DecoratedParser<'a> {
    type Item = Event<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let result = 'result: {
            if let Some(ref mut iter) = self.current_definition {
                let res = iter.next();
                if res == None {
                    self.current_definition = None;
                } else {
                    break 'result res;
                }
            }

            let ev = self.events.next();
            match ev {
                Some(Event::FootnoteReference(tag)) => {
                    let def = self.definitions.iter().find(|def| {
                        match def.get(0).expect("The definition is not empty") {
                            Event::Start(cmark::Tag::FootnoteDefinition(def_tag)) => {
                                tag == *def_tag
                            }
                            _ => false,
                        }
                    });
                    if let Some(def) = def {
                        self.current_definition = Some(def.clone().into_iter());
                    }
                    Some(Event::FootnoteReference(tag))
                }
                ev => ev,
            }
        };

        match result {
            Some(Event::FootnoteReference(_)) => Some(Event::Html(cmark::CowStr::Borrowed(
                r#"<span class="sidenote-anchor"></span>"#,
            ))),
            Some(Event::Start(cmark::Tag::FootnoteDefinition(_))) => {
                assert!(!self.in_sidenote);
                self.in_sidenote = true;
                Some(Event::Html(cmark::CowStr::Borrowed(
                    r#"<span class="sidenote">"#,
                )))
            }
            Some(Event::End(cmark::TagEnd::FootnoteDefinition)) => {
                assert!(self.in_sidenote);
                self.in_sidenote = false;
                Some(Event::Html(cmark::CowStr::Borrowed(r#"</span>"#)))
            }
            Some(Event::Start(cmark::Tag::Paragraph)) if self.in_sidenote => self.next(),
            Some(Event::End(cmark::TagEnd::Paragraph)) if self.in_sidenote => self.next(),
            ev => ev,
        }
    }
}

fn decorate_footnote<'a, T>(parser: T) -> error::Result<DecoratedParser<'a>>
where
    T: 'a + Iterator<Item = Event<'a>>,
{
    Ok(DecoratedParser::new(parser))
}

impl Markdown {
    pub fn parse(&self, content: &str) -> Result<String> {
        let mut buf = String::new();
        let options = cmark::Options::ENABLE_FOOTNOTES
            | cmark::Options::ENABLE_TABLES
            | cmark::Options::ENABLE_STRIKETHROUGH
            | cmark::Options::ENABLE_TASKLISTS;
        let parser = cmark::Parser::new_ext(content, options);
        let parser = decorate_markdown(parser, self.syntax.clone(), self.theme.as_deref())?;
        let parser = decorate_footnote(parser)?;
        cmark::html::push_html(&mut buf, parser);
        Ok(buf)
    }
}
