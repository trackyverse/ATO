# What’s inside an ATO

## Slot based data storage

The Animal Tracking Object (ATO) is an S4 object made of several slots.
These were designed to be broadly applicable to tracking data of any
kind (e.g. acoustic telemetry, satellite, radio). The slots are
interconnected, so the ATO automatically checks e.g. which of your tags
were detected or observed (different things, see below) throughout the
study.

| slot | defining class | other classes | optional classes   |
|:-----|:---------------|:--------------|:-------------------|
| @det | ATO_det        | data.frame    | data.table, tibble |
| @dep | ATO_dep        | data.frame    | data.table, tibble |
| @tag | ATO_tag        | data.frame    | data.table, tibble |
| @ani | ATO_ani        | data.frame    | data.table, tibble |
| @obs | ATO_obs        | data.frame    | data.table, tibble |
| @log | ATO_log        | data.frame    | data.table, tibble |
| @tbl | ATO_tbl        | character     | \-                 |

### @ani

The animals slot contains information on the animals tracked.

| column name (snake case) | column type | mandatory | short description                          |
|:-------------------------|:------------|:----------|:-------------------------------------------|
| capture_lat              | numeric     | no        | latitude of the capture                    |
| capture_lon              | numeric     | no        | longitude of the capture                   |
| release_location         | character   | yes       | place where the animal was released        |
| release_datetime         | POSIXct     | yes       | date and time when the animal was released |
| release_lat              | numeric     | no        | latitude of the release                    |
| release_lon              | numeric     | no        | longitude of the release                   |
| tz                       | character   | yes       | timezone of the datetime data              |
| …                        | any         | no        | non-standard column names are allowed      |

### @tag

The tags slot contains information on the transmitters that were used to
track the animals.

| column name (snake case) | column type | mandatory | short description                                              |
|:-------------------------|:------------|:----------|:---------------------------------------------------------------|
| manufacturer             | character   | no        | company that made the tag                                      |
| model                    | character   | no        | model of the tag                                               |
| power_level              | numeric     | no        | power level of the tag                                         |
| ping_rate                | numeric     | no        | ping rate of the tag                                           |
| ping_variation           | numeric     | no        | +- variation added to individual pings                         |
| serial                   | character   | no        | serial code of the tag                                         |
| transmitter              | character   | yes       | transmitter code emitted by the tag                            |
| activation_datetime      | POSIXct     | no        | activation date                                                |
| battery_life             | numeric     | no        | battery life, in days                                          |
| sensor_type              | character   | no        | sensor type associated with the transmitter (e.g. temperature) |
| sensor_unit              | character   | no        | units of the sensor data collected (e.g. C)                    |
| animal                   | character   | no        | name of the animal that received the tag                       |
| …                        | any         | no        | non-standard column names are allowed                          |

The `animal` column will be matched against the animal names provided in
the @ani slot.

### @dep

The deployments slot contains information on autonomous, stationary
receivers that were deployed to listen to the transmitters.

| column name (snake case) | column type | mandatory                        | short description                                                                       |
|:-------------------------|:------------|:---------------------------------|:----------------------------------------------------------------------------------------|
| receiver_manufacturer    | character   | missing in R code                | company that made the receiver                                                          |
| receiver_model           | character   | no                               | model of the receiver                                                                   |
| receiver_serial          | character   | no if transmitter provided       | serial code of the receiver (this is the column used to match deployments)              |
| receiver_codeset         | character   | no                               | codeset of the receiver                                                                 |
| deploy_location          | character   | yes                              | name of the location where the receiver was deployed                                    |
| deploy_datetime          | POSIXct     | yes                              | date and time of the deployment (yyyy-mm-dd hh:mm:ss)                                   |
| deploy_lat               | numeric     | no                               | latitude of the deployment                                                              |
| deploy_lon               | numeric     | no                               | longitude of the deployment                                                             |
| deploy_z                 | numeric     | no                               | depth of the deployment (as measured from the reference surface of the waterbody)       |
| recover_datetime         | POSIXct     | yes                              | date and time of the recovery (yyyy-mm-dd hh:mm:ss)                                     |
| recover_lat              | numeric     | no                               | latitude of the recovery                                                                |
| recover_lon              | numeric     | no                               | longitude of the recovery                                                               |
| transmitter              | character   | no if receiver_serial provided   | Can either be a beacon located inside a receiver, or a stationary reference transmitter |
| transmitter_manufacturer | character   | no                               | company that made the transmitter                                                       |
| transmitter_ping_rate    | numeric     | no (yes if transmitter provided) | ping rate of the transmitter                                                            |
| transmitter_model        | character   | no                               | model of the transmitter                                                                |
| transmitter_serial       | character   | no                               | serial code of the transmitter                                                          |
| tz                       | character   | yes                              | timezone of the datetime data                                                           |
| …                        | any         | no                               | non-standard column names are allowed                                                   |

### @det

The detections slot contains the detection extracts obtained from the
stationary receivers deployed.

| column name (snake case) | column type | mandatory | short description                                          |
|:-------------------------|:------------|:----------|:-----------------------------------------------------------|
| datetime                 | POSIXct     | yes       | date and time of the detection (yyyy-mm-dd hh:mm:ss)       |
| frac_second              | numeric     | no        | fraction of the second when the detection was received     |
| receiver_serial          | character   | yes       | serial number of the receiver that picked up the detection |
| transmitter              | character   | yes       | transmitter that was picked up                             |
| sensor_value             | numeric     | no        | respective sensor value                                    |
| tz                       | character   | yes       | timezone of the datetime data                              |
| …                        | any         | no        | non-standard column names are allowed                      |

The `receiver_serial` column will be matched to the receivers listed in
the @dep slot.

### @obs

The observations slot contains records of direct and indirect
observations of an animal or tag. For example, a photo of the tail of a
whale is a **direct** observation of the **animal**. A reported capture
of a fish by an angler is also a **direct** observation of the
**animal**. On the other hand, a satellite-estimated position of a
transmitter is an **indirect** observation of a **tag**. A detection of
a tag using a mobile receiver is also an **indirect** observation of a
**tag**. The choice between direct and indirect observation types
declares the confidence you have on that observation. This information
may be used by modelling tools to determine which positions to rely on
more heavily.

| column name (snake case) | column type | mandatory                                   | short description                                              |
|:-------------------------|:------------|:--------------------------------------------|:---------------------------------------------------------------|
| animal                   | character   | no if not terminal and transmitter provided | name/code of the observed animal                               |
| transmitter              | character   | no if animal provided                       | transmitter detected                                           |
| type                     | character   | no                                          | type of observation (e.g. manual tracking, visual, etc)        |
| terminal                 | logical     | yes                                         | does this observation mark the end of tracking for this animal |
| location                 | character   | yes                                         | location of the observation                                    |
| datetime                 | POSIXct     | yes                                         | date and time of the observation                               |
| lat                      | numeric     | no                                          | latitude of the observation                                    |
| lon                      | numeric     | no                                          | longitude of the observation                                   |
| …                        | any         | no                                          | non-standard column names are allowed                          |
| tz                       | character   | yes                                         | timezone of the datetime data                                  |

The `animal` column is matched to the animal names in the @ani slot,
while the `tag` column is matched to the `transmitter` column in the
@tag slot. Observations typically only use one of the two columns.

### @log

The log slot keeps track of the operations performed to the ATO since it
was initialized to its current state. It is a great tool both for
debugging and for increasing the reproducibility of animal tracking
analyses.

| column name (snake case) | column type | mandatory                                   | short description                                              |
|:-------------------------|:------------|:--------------------------------------------|:---------------------------------------------------------------|
| animal                   | character   | no if not terminal and transmitter provided | name/code of the observed animal                               |
| transmitter              | character   | no if animal provided                       | transmitter detected                                           |
| type                     | character   | no                                          | type of observation (e.g. manual tracking, visual, etc)        |
| terminal                 | logical     | yes                                         | does this observation mark the end of tracking for this animal |
| location                 | character   | yes                                         | location of the observation                                    |
| datetime                 | POSIXct     | yes                                         | date and time of the observation                               |
| lat                      | numeric     | no                                          | latitude of the observation                                    |
| lon                      | numeric     | no                                          | longitude of the observation                                   |
| …                        | any         | no                                          | non-standard column names are allowed                          |
| tz                       | character   | yes                                         | timezone of the datetime data                                  |

Next: [How do I create an
ATO?](https://ato.trackyverse.org/articles/creating-an-ATO.html)
