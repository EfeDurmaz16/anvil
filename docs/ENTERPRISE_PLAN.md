# Anvil Enterprise-Grade B2B Modernizasyon Planı (Bankacılık + Telekom)

Bu doküman, Anvil’in “legacy sistem aynı fonksiyonellikle devam etsin” hedefiyle (on‑prem/air‑gapped veya müşteri VPC) enterprise-grade bir B2B ürüne evrilmesi için uygulanabilir bir 90 günlük planı ve ürün kararlarını içerir.

## 1) Ürün Hedefi

Anvil, legacy kodu (öncelik: COBOL) modern dillere (öncelik: TypeScript, Python, Go) dönüştürürken **davranış eşdeğerliğini kanıt paketleriyle** doğrulayan bir modernizasyon platformudur.

Satın aldıran değer: “codegen” değil; **kanıtlanmış doğruluk + izlenebilirlik + operasyonel çalıştırılabilirlik**.

## 2) Sabit Kararlar (Pilot için)

### 2.1 Record/Replay Kaynağı (Online akışlar)
V1: **HTTP/JSON gateway/middleware logları** (müşteri VPC/on‑prem’de).

- Üretimde (veya staging’de) request/response body + correlation id kaydı.
- Replay aynı fixture seti ile “yeni hedef runtime” üzerinde koşar.

Not: CICS/3270 record/replay ve terminal akışları Phase 4+ kapsamı olarak ele alınır (çok daha büyük).

### 2.2 DB Stratejisi
Pilot: **kopya DB** üzerinde koşum + test bitince prod cutover.

- Run başı: seed/snapshot sabitlenir.
- Run sonu: kritik tablolar için diff raporu üretilir.
- Nondeterministic alanlar (timestamp/sequence/id) normalize/whitelist ile yönetilir.

### 2.3 Pilot “iş akışı” (karma sistem)
Telekom‑odaklı, net ve ölçülebilir bir kapsam:

- Online (record/replay):
  - `BillInquiry` (read-heavy)
  - `BillPayment` (write + idempotency)
- Batch:
  - `DailyReconciliation` (gün sonu hesaplaşma/rapor)

Bu kapsam, hem API davranışı hem DB yan etkileri hem de batch çıktılarını “equivalence gate” ile ölçmeye yeterli.

## 3) “Aynı Fonksiyonellik” Tanımı (Kabul Kriterleri)

### 3.1 Online (API)
- Aynı request → aynı response (status/body/hata kodları) **normalize kuralları sonrası**.
- Yan etki: DB write set’i aynı (kritik tablolar + alanlar).
- Performans: belirlenen budget içinde (p95 latency, throughput).

### 3.2 Batch
- Aynı input → aynı output (hash veya normalize diff).
- DB etkileri: aynı tablo/alan değişimleri.

### 3.3 Proof Pack (her job sonunda)
Her koşum sonunda tek klasörde:

- IR/graph çıktıları
- build/test logları
- api diff raporu
- db diff raporu
- performans raporu
- judge verdict’leri (LLM varsa) + parse edilmiş sonuç

## 4) 90 Gün Roadmap

### Phase 1 — On‑prem/VPC Foundation (Hafta 0–2)
Amaç: air‑gapped/VPC ortamda deterministik çalıştırma.

- Offline build/release stratejisi (vendor/SBOM/sha)
- Deploy paketleri (compose/helm) + örnek config
- Local LLM endpoint’leri ile çalışma (OpenAI-compatible base_url)
- Log standardı + job/correlation id

**Kabul:** İnternetsiz ortamda stack ayakta + örnek job çalışır + proof pack üretimi başlar.

### Phase 2 — Accuracy Gate / Proof Pack (Hafta 2–6)
Amaç: satın aldıran “kanıt” katmanı.

- Fixture formatı (JSONL) + normalizer rules
- Replay runner (online) + batch runner
- DB diff v1 (kritik tablolar + normalize)
- Gate skoru: fixture pass rate + db diff + compile/typecheck + perf budget

**Kabul:** Pilot fixture setinde hedef dil runtime’larında gate raporu üretir (pass/fail net).

### Phase 3 — Multi‑Target Quality (Hafta 6–12)
Amaç: TS/Python/Go’da aynı kalite standardı.

- Target runtime adapter’ları (Node/Python/Go runner)
- Decimal/encoding stratejisi (para: kesinlik)
- Profile’lar (`--profile mixed|batch|service`)
- Regresyon + perf budget ve otomasyon

**Kabul:** En az 1 pilot akış için “eşdeğerlik” %X üstü; failure bucket’ları aksiyonlanabilir.

## 5) 90 Gün Dışı (Phase 4+)
- CICS/3270 terminal record/replay
- Çok tenant SaaS
- “Her legacy dili her target’a” iddiası

## 6) Target Dil Stratejisi (Konfigürasyonsuz Genişleme)
Hedef: Müşteri yeni bir target dil istediğinde **Anvil tarafında ekstra konfigürasyon gerektirmeden** eklenebilsin.

V1 yaklaşımı:
- Her target plugin scaffold aşamasında köke `anvil.manifest.json` üretir.
- Manifest, “compile/typecheck” ve “fixture runner” komutlarını tarif eder.
- Harness, dili “hardcode” etmek yerine manifest’e göre çalıştırır.

Bu sayede yeni bir dil eklemek için:
1) Sadece yeni bir `TargetPlugin` implement edilir.
2) Plugin, kendi dilinde minimal bir “fixture runner” üretir (stdin→stdout JSON).
3) Harness/CLI/Temporal kodunda ek değişiklik gerekmez.

## 7) Commit Bazlı Uygulama Planı (90 gün)
Bu repo’da ilerleme “küçük, test-geçen commit’ler” olarak yapılır:

1) Enterprise plan + harness core (fixture schema, normalize, diff, proof summary)
2) Manifest tabanlı runner + PII redaction + SBOM (proof pack genişletme)
3) CLI entegrasyonu: `anvil harness` ve `anvil proof-pack` komutları
4) Temporal entegrasyonu: Harness activity + gate score
5) Target plugin’lerde `anvil.manifest.json` + standart runner üretimi (TS/Python/Go)
6) Core refactor: plugin file discovery, treesitter build tags, uuid stdlib, graph/vector package split
7) Deploy + offline build: `.env.example`, Docker/compose, helm skeleton, vendor/SBOM/sha
