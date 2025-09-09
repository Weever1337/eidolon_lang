use eidolon_lang::interpreter::evaluate;
use eidolon_lang::interpreter::value::EidolonValue;
use eidolon_lang::parse_eidolon_source;
use jni::objects::{GlobalRef, JClass, JObject, JString};
use jni::sys::{jlong, jstring};
use jni::JNIEnv;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

struct EngineState {
    log_callback: Option<GlobalRef>,
}

lazy_static! {
    static ref ENGINES: Mutex<HashMap<u64, Arc<Mutex<EngineState>>>> = Mutex::new(HashMap::new());
}
static NEXT_ENGINE_ID: AtomicU64 = AtomicU64::new(1);

#[unsafe(no_mangle)]
pub extern "system" fn Java_io_nexus_eidolon_NexusEngine_nativeInit<'local>(
    mut _env: JNIEnv<'local>,
    _class: JClass<'local>,
) -> jlong {
    let engine_id = NEXT_ENGINE_ID.fetch_add(1, Ordering::SeqCst);
    let state = EngineState { log_callback: None };
    let mut engines = ENGINES.lock().unwrap();
    engines.insert(engine_id, Arc::new(Mutex::new(state)));
    engine_id as jlong
}

#[unsafe(no_mangle)]
pub extern "system" fn Java_io_nexus_eidolon_NexusEngine_nativeDestroy<'local>(
    mut _env: JNIEnv<'local>,
    _class: JClass<'local>,
    engine_id: jlong,
) {
    let mut engines = ENGINES.lock().unwrap();
    engines.remove(&(engine_id as u64));
}

#[unsafe(no_mangle)]
pub extern "system" fn Java_io_nexus_eidolon_NexusEngine_nativeEvaluate<'local>(
    mut env: JNIEnv<'local>,
    _class: JClass<'local>,
    _engine_id: jlong,
    source: JString<'local>,
) -> jstring {
    let source_code: String = env.get_string(&source).expect("Couldn't get java string!").into();

    let globals = HashMap::<String, EidolonValue>::new();

    let result_str = match parse_eidolon_source(&source_code) {
        Ok(ast) => {
            match evaluate(&ast, &globals) {
                Ok(result) => result.to_string(),
                Err(e) => format!("ERROR: [Runtime Error on line {}] {}", e.line, e.message),
            }
        }
        Err(e) => format!("ERROR: [Parse Error on line {}] {}", e.line, e.message),
    };

    let output = env.new_string(result_str).expect("Couldn't create java string!");
    output.into_raw()
}

#[unsafe(no_mangle)]
pub extern "system" fn Java_io_nexus_eidolon_NexusEngine_nativeSetLogCallback<'local>(
    env: JNIEnv<'local>,
    _class: JClass<'local>,
    engine_id: jlong,
    callback: JObject<'local>,
) {
    let engines = ENGINES.lock().unwrap();
    if let Some(state_arc) = engines.get(&(engine_id as u64)) {
        let mut state = state_arc.lock().unwrap();
        if callback.is_null() {
            state.log_callback = None;
        } else {
            let global_ref = env.new_global_ref(callback).unwrap();
            state.log_callback = Some(global_ref);
        }
    }
}