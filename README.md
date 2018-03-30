Ambiata [Weather Observations](https://github.com/ambiata/interview/blob/master/weather.md) coding exercise.

**Done**: test data generation and flight stats.<br/>
**Not done**: normalized data output, support for out-of-order records.

**generate_test_data.fsx** — script for generating test data.<br/>
**build.fsx** — FAKE tasks (Build and RunTests).<br/>
**AmbiataWeather** — main application, --help for usage info.


Before build, open console and invoke **paket restore** command in the project root to download referenced libraries and tools.
