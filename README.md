# Report for Mass-Update of Data Provider Content (e.g. REGUVIS).

## Core Functionalities
  - Background functionality
  - Sequential call of SAP standard upload reports
  - REGEX/PCRE file name evaluation
  - E-Mail functionality

## Manual Activities
### Data Dictionary Objects
#### Table ZGTS_DP_UPLOAD
Steuerung Datenupload Ext. Datenprovider
| Field    | Data Element        |
| -------- | ------------------- |
| .Include | `ZGTS_DP_UPLOAD_PK_S` |
| .Include | `ZGTS_DP_UPLOAD_S`    |

#### Structure ZGTS_DP_UPLOAD_PK_S
Steuerung Datenupload Ext. Datenprovider - PK mit Mandant
| Field    | Data Element         |
| -------- | -------------------- |
| `MANDT`  | `MANDT`              |
| .Include | `ZGTS_DP_UPLOAD_K_S` |

#### Structure ZGTS_DP_UPLOAD_K_S
Steuerung Datenupload Ext. Datenprovider - PK ohne Mandant
| Field     | Data Element       |
| --------- | ------------------ |
| `CONTENT` | `ZGTS_DP_CONTENT`  |

#### Structure ZGTS_DP_UPLOAD_S
Steuerung Datenupload Ext. Datenprovider - Daten
| Field   | Data Element          |
| ------- | --------------------- |
| `REGEX` | `ZGTS_REGEX`          |
| `PROCS  | `ZGTS_DP_PROCESS_OPT` |

#### Data Element ZGTS_DP_CONTENT
  - Description: Contenttyp Ext. Datenprovider
  - Domain: `ZGTS_DP_CONTENT`

#### Data Element ZGTS_REGEX
  - Description: REGEX Statement
  - Domain: `SCI_REGEX_LINE`

#### Data Element ZGTS_DP_PROCESS_OPT
  - Description: Datenprovider Verarbeitungsoption
  - Domain: `ZGTS_DP_PROCESS_OPT`

#### Domain ZGTS_DP_CONTENT
  - Description: Contenttyp Ext. Datenprovider
  - Data Type: CHAR 10
  - Values:
    - `ALNDE` Ausfuhrlistennummern DE
    - `CCLUS` Commerce Control List US
    - `DUAEU` Dual-Use Listen EU
    - `MEADE` Maßnahmen DE
    - `MEAEU` Maßnahmen EU
    - `RETEU` Retarifierung EU
    - `RETUK` Retarifierung UK
    - `SPLDE` SPL Listen DE
    - `SPLUS` SPL Listen US
    - `TAREU` Tarifstamm EU
    - `TARUK` Tarifstamm UK
    - `TARUS` Tarifstamm US
  
### Domain ZGTS_DP_PROCESS_OPT
  - Description: Datenprovider Upload: Verarbeitungsoptionen
  - Data Type: CHAR 1
  - Values:
    - `A` Alle
    - `N` Neueste

### Report Text Symbols:
  - `001` Ein neuer Datenupload wurde gestartet.
  - `002` Simulationsmodus:
  - `003` Folgende Dateien wurden hochgeladen:
  - `999` Upload beendet.
  - `B01` Datenprovider
  - `B02` Datentyp
  - `B03` Uploadoptionen
  - `B04` Anwenderlog
  - `B05` Parameter

## Sample Table Content
| Content | REGEX                                            | PROCS |
| ------- | ------------------------------------------------ | ----- |
| `ALNDE` | `^DE-DUALUSE-1A_I_\d+\.xml$`                     | `N`   |
| `CCLUS` | `^US-CCL_I_\d+\.xml$`                            | `N`   |
| `DUAEU` | `^EU-DUALUSE-ELAN_I_\d+\.xml$`                   | `N`   |
| `MEADE` | `^DE-MEASURES_\d{5}_I_\d+\.xml$`                 | `A`   |
| `MEAEU` | `^EU-MEASURES_\d{5}_I_\d+\.xml$`                 | `A`   |
| `RETEU` | `^EU-EXPCODE-RE_I_\d+\.xml$`                     | `N`   |
| `RETUK` | `^GB-IMPCODE-RE_I_\d+\.xml$`                     | `N`   |
| `SPLDE` | `^BL((AL)?(DIV)?)_I_\d+\.xml$`                   | `N`   |
| `SPLUS` | `^((BLUS)?(DJMAE)?(DJSCO)?(DJTAE)?)_I_\d+\.xml$` | `N`   |
| `TAREU` | `^EU-EXPCODE_I_\d+\.xml$`                        | `N`   |
| `TARUK` | `^GB-IMPCODE_I_\d+\.xml$`                        | `N`   |
| `TARUS` | `^US-IMPCODE_I_\d+\.xml$`                        | `N`   |
