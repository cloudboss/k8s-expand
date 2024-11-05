use std::cell::RefCell;
use std::collections::HashMap;

use criterion::{criterion_group, criterion_main, Criterion};
use pprof::criterion::{Output, PProfProfiler};

use k8s_expand::{expand, mapping_func_for};

pub fn expand_bench(c: &mut Criterion) {
    struct NameValue<'a> {
        name: &'a str,
        value: &'a str,
    }

    let envs = vec![
        NameValue {
            name: "FOO",
            value: "bar",
        },
        NameValue {
            name: "ZOO",
            value: "$(FOO)-1",
        },
        NameValue {
            name: "BLU",
            value: "$(ZOO)-2",
        },
    ];

    let declared_env: HashMap<String, RefCell<String>> = HashMap::from_iter(
        vec![("FOO", "bar"), ("ZOO", "$(FOO)-1"), ("BLU", "$(ZOO)-2")]
            .into_iter()
            .map(|(k, v)| (k.to_string(), v.to_string().into())),
    );

    let maps = vec![&declared_env];
    let mapping = mapping_func_for(&maps);

    c.bench_function("expand", |b| {
        b.iter(|| {
            for entry in envs.iter() {
                let vrc = declared_env.get(entry.name).unwrap();
                *vrc.borrow_mut() = expand(entry.value.into(), &mapping);
            }
        })
    });
}

criterion_group! {
    name = benches;
    config = Criterion::default().with_profiler(PProfProfiler::new(100, Output::Flamegraph(None)));
    targets = expand_bench
}
criterion_main!(benches);
