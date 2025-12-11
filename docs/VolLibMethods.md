## Main Entry Methods

### `VOLINIT` - (volinit.f)
old mian volume calculation method. This was used by versions of Cruise Processing up till early 2025?.
The core logic of this method is to call the various volume calculation methods based on the volume equation. At the end of the method there are some additional check and ajustments. And then log weights are calculated. 


#### Calls
 - `FIAEQ2NVELEQ`
 - `FIAVOLUME`
 - `VOLEQDEF`
 - `PROFILE`
 - `R4VOL`
 - `PNWTARIF`
 - `R6VOL`
 - `BLMVOL`
 - `GETFCLASS`
 - `BIA_Behres_Hyperobla`
 - `VolEq_Johnson`
 - `R9CLARK`
 - `R8CLARK`
 - `R8VOL`
 - `R10HTS`
 - `R10VOL`
 - `R12VOL`
 - `BH_NonSawPP`
 - `DVEST`
 - `HANN_PP`
 - `CALCDIA2`
 - `RAILEVOL`
 - `GetRegnWF`
 - `CruiseLogWt`

### `VOLINITNVB` - (volinit.f)

#### Calls
 - `JENKINS`
 - `NVB_RefSpcData`
 - `GetRegnWF`
 - `NVBC`
 - `VOLINIT`
 - `NVB_DefaultEq`
 - `NVB_RefSpcData`
 - `WOODLAND_BIO`
 - `JENKINS`
 - `NVB_EcoProv`
 - `NVB_BrchRem`
 - `DecayDenProp`
 - `GetRegnWF`


## Volume Calculation Logic

### `PROFILE` - (profile.f)

#### Calls
 - `MRULES`
 - `FWINIT`
 - `VOLINTRP`
 - `R10HTS`
 - `TOP6LEN`
 - `R6VOL3`
 - `TCUBIC`
 - `TAPERMODEL`
 - `MERLEN`
 - `FIREWOOD`
 - `NUMLOG`
 - `SEGMNT`
 - `GETDIB`
 - `SCRIB`
 - `INTL14`
 - `VOLRATIO`


### `JENKINS` - (jenkins.f)

#### Includes
 - `wdbkwtdata.inc`

 #### Data
  - `COEF` - Coefficient for above ground total biomass for 10 species group
  

#### Calls
 - `RAILEVOL`
 - `WOODDEN`

 ## Data files

### `beqinfo.inc` - not used?, is in vfproj
List of biomass equations and discriptions
- created by NBEL Tester program
#### Arrays
##### BEQN
 - array length: 84
##### BEQREF
 - array length: 84

### `beqrefinfo.inc`
List of discriptions for three character biomass equation codes
Used in `crzbiomass.f`
Defines type `REFINFO`  
#### Arrays
##### BEQREF
 - array length: 203

 ### `bioeqcoef.inc` 
 - created by NBEL Tester program
 Used in `calcbiomass.f`
 #### BEQCOEF
 - array demensions:6059,6

 ### `bioeqinfo.inc`
  - created by NBEL Tester program
  Defines type `EQINFO`
  Used in `calcbiomass.f`
#### BIOMSEQ
 - array length: 500

### `dist_ecoprov.inc`
national forest district crosswalk with EcoProvince
Used in `nsvb.f`
#### DistProv
 - demensions: 2,50

 ### `fiabioeq.inc`
 - created by NBEL Tester program
 Used by `calcbiomass.f`

 #### FIABEQ
 - length: 1580

 ### `r8cfo.inc`
 R8 Clark coef for estimating DOB at 17.3 ft from DBH and total, 4", 7/9" HT
 #### R8CFO
 - demensions: 9,182

 ### `r8clist.inc`
 Used in `r8prep.f`
 #### COFARR
 - demensions:162,10

### `r8clkcoef.inc`
Used in `r8clkdib.f`
Used in `r8prep.f`
#### DIBMEN
- 49,3
#### TOTAL
- 49,7
#### NINE
 - 34,6
#### SEVEN
 - 15,6
 #### FOUR
 - 49,6
 #### TOPRAT
 - 3
 #### OTOTOAL
 - 49,7

### `r8dib.inc` - not used
Used in `r8clkdib.f`
Used in `r8prep.f`
Used in `r8vol2.f`


### `r8vlist.inc`
Used in `r8vol1.f`

### `r9coeff.inc`
Used in `r9clark.f`
Used in `r9clark_mar10.f`
Used in `r9clarkbh.f`

### `regndftdata.inc`
Used in `crzbiomass.f`
Used in `nsvb.f`

### `tables1.inc` - `tables11.inc`
Used in `nsvb.f`

### `wdbkwtdata_20190807.inc` - not used

### `wdbkwtdata.inc`
Used in `calcbiomass.f`
Used in `crzbiomass.f`
Used in `fia_nw.f`
Used in `jenkins.f`
Used in `nsvb.f`

## Index

### Volinit.f
 - VOLINIT lines 1-678
 - VOLINITNVB lines 682-1025
 - CruiseLogWt lines 1029-1047

 ### profile.f
 - PROFILE lines 1-864
 - TCUBIC lines 883-974
 - MERLEN
 - GETDIB
 - TOP6LEN
 - VOLRATIO
 - TAPERMODEL
 - FWSMALL
 - VOLINTRP
 - R10HTS
 - FIREWOOD