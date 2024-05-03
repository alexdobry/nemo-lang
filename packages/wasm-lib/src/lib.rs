use backend::codegen::codegen;
use frontend::highlight;
use wasm_bindgen::prelude::*;
use wasmprinter;

#[wasm_bindgen(getter_with_clone)]
#[derive(Clone)]
pub struct Diagnostic {
    pub message: String,
    pub start: usize,
    pub end: usize,
}

#[wasm_bindgen(getter_with_clone)]
#[derive(Clone)]
pub struct Highlight {
    pub start: usize,
    pub end: usize,
    // TODO wasteful
    pub kind: String,
}

impl Highlight {
    pub fn new(highlight: highlight::Highlight) -> Self {
        Highlight {
            kind: format!("{:?}", highlight.kind),
            start: highlight.range.start().into(),
            end: highlight.range.end().into(),
        }
    }
}

#[wasm_bindgen(getter_with_clone)]
pub struct CompileResult {
    pub wasm: Vec<u8>,
    pub wast: String,
    pub errors: Vec<Diagnostic>,
    pub highlights: Vec<Highlight>,
}

#[wasm_bindgen]
pub fn compile(input: &str) -> CompileResult {
    let check_result = frontend::run_frontend(input);
    let highlights = highlight::highlight(&check_result.parse, &check_result.names)
        .into_iter()
        .map(Highlight::new)
        .collect();
    let (name_map, result) = match check_result.ir {
        Some(ir) if check_result.errors.is_empty() => {
            let wasm = codegen(ir, &check_result.name_map);
            (check_result.name_map, Ok(wasm))
        }
        _ => (check_result.name_map, Err(check_result.errors)),
    };

    match result {
        Ok(wasm) => {
            let wast =
                wasmprinter::print_bytes(&wasm).unwrap_or_else(|e| format!("internal error: {e}"));
            CompileResult {
                wasm,
                wast,
                highlights,
                errors: vec![],
            }
        }
        Err(errors) => CompileResult {
            wasm: vec![],
            wast: "".to_string(),
            highlights,
            errors: errors
                .into_iter()
                .map(|e| Diagnostic {
                    message: e.message(&name_map),
                    start: e.at().start().into(),
                    end: e.at().end().into(),
                })
                .collect(),
        },
    }
}
