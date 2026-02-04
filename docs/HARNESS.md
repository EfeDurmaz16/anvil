# Harness (Record/Replay) ve Proof Pack

Bu doküman Anvil’in “aynı fonksiyonellik” hedefi için harness yaklaşımını özetler.

## Kavramlar

- **Fixture (JSONL):** Kayıtlı bir senaryo (HTTP veya batch) satır bazlı JSON Lines formatında saklanır.
- **Manifest:** Generated target projenin kökünde `anvil.manifest.json` bulunur ve harness’in compile/run komutlarını tarif eder.
- **Proof Pack:** Koşumun kanıt paketi. V1: `summary.json` (pass/fail + fixture sonuçları).

## Fixture formatı (V1)

Her satır bir fixture:

```json
{"kind":"http","name":"bill_inquiry","correlation_id":"...","http":{"method":"POST","path":"/bill/inquiry","expected_status":200,"expected_body":{"ok":true}}}
```

## Manifest formatı (V1)

`anvil.manifest.json` örneği:

```json
{
  "version": "1",
  "language": "typescript",
  "compile": [
    { "cmd": "tsc", "args": ["-p", "tsconfig.json"] }
  ],
  "run_fixture": { "cmd": "node", "args": ["dist/anvil_runner.js"] }
}
```

Runner programı stdin’den fixture JSON alır, stdout’a `ActualOutput` JSON yazar.

## CLI kullanımı

Fixture’ları target kod üzerinde çalıştır:

```bash
./anvil harness run \
  --fixtures fixtures.jsonl \
  --code /path/to/generated-project \
  --output /tmp/anvil-proof
```

Notlar:
- `--code` dizininde `anvil.manifest.json` bulunmalıdır.
- `--output` verilirse `/tmp/anvil-proof/summary.json` yazılır.

