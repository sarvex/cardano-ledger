window.BENCHMARK_DATA = {
  "lastUpdate": 1682352009425,
  "repoUrl": "https://github.com/input-output-hk/cardano-ledger",
  "entries": {
    "Haskell Benchmark": [
      {
        "commit": {
          "author": {
            "email": "alexey.kuleshevich@iohk.io",
            "name": "Alexey Kuleshevich",
            "username": "lehins"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "d9874d781af29b911c94b0e34180aebb80915054",
          "message": "Merge pull request #3379 from input-output-hk/lehins/release-process\n\nRelease process documentation",
          "timestamp": "2023-04-24T18:56:25+03:00",
          "tree_id": "73c7e32ad58b2f7489ea943efc00825a35f00d6c",
          "url": "https://github.com/input-output-hk/cardano-ledger/commit/d9874d781af29b911c94b0e34180aebb80915054"
        },
        "date": 1682352002544,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/ShelleyEra C_Crypto",
            "value": 0.000070434620380437,
            "unit": "Nanoseconds",
            "range": 4.915856545452598e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AllegraEra C_Crypto",
            "value": 0.00007312580091282237,
            "unit": "Nanoseconds",
            "range": 3.4179357452329107e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/MaryEra C_Crypto",
            "value": 0.00008756179277019996,
            "unit": "Nanoseconds",
            "range": 2.153852023550486e-7
          },
          {
            "name": "applyTxBenchmarks/ApplyTxInEra/AlonzoEra C_Crypto",
            "value": 0.00011744084852537141,
            "unit": "Nanoseconds",
            "range": 1.8479250998568282e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/ShelleyEra C_Crypto",
            "value": 0.000018437632581716718,
            "unit": "Nanoseconds",
            "range": 1.2927507414883955e-7
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AllegraEra C_Crypto",
            "value": 0.000029941889418997924,
            "unit": "Nanoseconds",
            "range": 8.422556197103803e-8
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/MaryEra C_Crypto",
            "value": 0.00003112664250057103,
            "unit": "Nanoseconds",
            "range": 6.121354521009783e-8
          },
          {
            "name": "applyTxBenchmarks/Deserialise Shelley Tx/AlonzoEra C_Crypto",
            "value": 0.000016340463174676506,
            "unit": "Nanoseconds",
            "range": 5.969201616069144e-8
          }
        ]
      }
    ]
  }
}