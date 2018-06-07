# Carbonara

Carbonara is essentially
[graphite-carbon](https://github.com/graphite-project/carbon),
[whisper](https://github.com/graphite-project/whisper) and the key APIs of
[graphite-web](https://github.com/graphite-project/graphite-web)
all rolled into a single (higher performance) server.

The goal of this project is exclusively to improve the horrible response times I
see on my Grafana charts when pulling from the python graphite-api
implementation. While aiming for perfect wire / file compatibility, I guarantee
nothing. Don't use this in production.

## What works

| Feature   |      Status      |  Comment |
| ----------|:----------------:|:---------|
| Metric Ingest | :heavy_check_mark: | Line format metrics are supported over tcp and udp |
| Whisper Files | :heavy_plus_sign: | Whisper files can be created, updated and read. However, there is no aggregation of metrics to secondary archives. |
| API | :heavy_plus_sign: | Metric search seems faithful to the original, rendering doesn't yet respect time ranges |
| Tests | :x: | Someday |

# License

Copyright 2018 Ross Schlaikjer

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
