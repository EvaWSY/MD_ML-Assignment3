# Purpose: Import and clean NYC SQF data from 2008-2016
# Date: October 2018
# Team: Andrea Hassler, Eva Wang, Alan Chen

# SOURCE LIBRARIES & FUNCTIONS
source('code/library_team.R')


# IMPORT RAW DATA
for (year in 2008:2016) {
    if (year < 2015) {
        filename <- paste0('data/input/', year, '.csv')
    } else {
        filename <- paste0('data/input/sqf-', year, '.csv')
    }

    print(paste0('READING IN: ', year))
    sqf.data <- as_tibble(read.csv(filename))

    # Variable cleaning
    names(sqf.data) <- str_to_lower(names(sqf.data))

    if (year == 2014) {
        # Fix levels to be Y/N only
        regex <- "(^pf_)|(^cs_)|(^rf_)|(^ac_)|(^sb_)"
        cols <- names(sqf.data)[grep(regex, names(sqf.data))]
        cols <- c(cols, 'contrabn', 'pistol', 'riflshot', 'asltweap', 'knifcuti', 'machgun', 'othrweap')

        for (col in cols) {
            sqf.data[[col]] <- recode_factor(sqf.data[[col]], '1'='Y', ' '='N')
        }
        sqf.data$adtlrept <- recode_factor(sqf.data$adtlrept, ' '='N')
        sqf.data$radio <- recode_factor(sqf.data$radio, '1'='Y', '0'='N')
    }

    print('STEP 0')
    # PARSE COLUMNS
    # 0) drop columns that won't be used, either because they're irrelevant or 
    # seem to be coded badly
    sqf.data <- sqf.data %>% select(-recstat, -officrid, -sumoffen, -compyear, -comppct)

    print('STEP 1')
    # 1) date and time (except for weird times: some timestops (2008-2009) beyond 24 hour)
    sqf.data <- sqf.data %>% mutate(datestop = sprintf("%08d", as.integer(datestop)),
                                timestop = sprintf("%04d", as.integer(timestop)),
                                timestamp = mdy_hm(paste(datestop, timestop))) %>% 
        select(-datestop, -timestop)

    print('STEP 2')
    # 2) location information and serial number
    sqf.data <- sqf.data %>% mutate(precinct = as.factor(pct), xcoord = as.integer(xcoord),
                                ycoord = as.integer(ycoord), serial = ser_num) %>% 
        select(-pct, -ser_num)

    print('STEP 3')
    # 3) recode y/n variables
    sqf.data <- sqf.data %>% mutate(frisked = recode.yn(frisked), 
                                searched = recode.yn(searched), 
                                extra.reports = recode.yn(adtlrept),
                                reason.explained = recode.yn(explnstp), 
                                others.stopped = recode.yn(othpers),
                                arrested = recode.yn(arstmade),
                                summons.issued = recode.yn(sumissue),
                                radio.run = recode.yn(radio)
    )

    print('STEP 4')
    # 4) recode other binary variables and drop irrelevant variables
    sqf.data <- sqf.data %>% mutate(inside = recode.io(inout), observation.period = perobs,
                                suspected.crime = convert.offense.code(detailcm),
                                officer.verbal = recode.0V(offverb),
                                officer.shield = recode.0S(offshld),
                                arrested.reason = convert.arrest.reasons(arstoffn)) %>% 
    select(-adtlrept, -explnstp, -othpers, -arstmade, -sumissue, -radio, -inout, -perobs, -detailcm, -offverb, -offshld, -arstoffn)
    
    print('STEP 5')
    # 5) clean up other variables 
    # location: recode 'P' (for Pedestrian, which occurs mainly after 2008) and blank as 'neither'.
    sqf.data <- sqf.data %>% mutate(location.housing = recode.factor(sqf.data$trhsloc, c('P', 'H', 'T'), c('neither', 'housing', 'transit')))
    sqf.data <- sqf.data %>% 
        mutate(location.housing = replace(location.housing, is.na(location.housing), 'neither')) %>% 
        select(-trhsloc)

    # period of stop (in minutes)
    sqf.data <- sqf.data %>% mutate(stop.length = perstop) %>% select(-perstop)

    # type of id and officer in uniform
    sqf.data <- sqf.data %>% 
        mutate(identification = recode.factor(typeofid, c('O','P','R','V'), c('other', 'photo', 'refused', 'verbal')), officer.uniform = recode.factor(offunif, c('M', 'N', 'Y'), c('N', 'N', 'Y')), officer.uniform = recode.yn(officer.uniform)) %>% 
        select(-typeofid, -offunif)

    print('STEP 6')
    # 6) physical force variables
    sqf.data <- sqf.data %>% mutate(force.hands = recode.yn(pf_hands),
                                force.wall = recode.yn(pf_wall),
                                force.ground = recode.yn(pf_grnd),
                                force.drawn = recode.yn(pf_drwep),
                                force.pointed = recode.yn(pf_ptwep),
                                force.baton = recode.yn(pf_baton),
                                force.handcuffs = recode.yn(pf_hcuff),
                                force.pepper = recode.yn(pf_pepsp),
                                force.other = recode.yn(pf_other)
                                ) %>% 
        select(-pf_hands, -pf_wall, -pf_grnd, -pf_drwep, -pf_ptwep, -pf_baton, -pf_hcuff,
           -pf_pepsp, -pf_other)

    print('STEP 7')
    # 7) primary circumstances of stop
    sqf.data <- sqf.data %>% mutate(stopped.bc.object = recode.yn(cs_objcs),
                                stopped.bc.desc = recode.yn(cs_descr),
                                stopped.bc.casing = recode.yn(cs_casng),
                                stopped.bc.lookout = recode.yn(cs_lkout),
                                stopped.bc.clothing = recode.yn(cs_cloth),
                                stopped.bc.drugs = recode.yn(cs_drgtr),
                                stopped.bc.furtive = recode.yn(cs_furtv),
                                stopped.bc.violent = recode.yn(cs_vcrim),
                                stopped.bc.bulge = recode.yn(cs_bulge),
                                stopped.bc.other = recode.yn(cs_other)) %>% 
        select(-cs_objcs, -cs_descr, -cs_casng, -cs_lkout, -cs_cloth, - cs_drgtr, -cs_furtv, -cs_vcrim, -cs_bulge, -cs_other)

    print('STEP 8')
    # 8) reasons for frisk
    sqf.data <- sqf.data %>% mutate(frisked.bc.suspected.crime = recode.yn(rf_vcrim),
                                frisked.bc.weapons = recode.yn(rf_othsw),
                                frisked.bc.attire = recode.yn(rf_attir),
                                frisked.bc.actual.crime = recode.yn(rf_vcact),
                                frisked.bc.noncompliance = recode.yn(rf_rfcmp),
                                frisked.bc.threats = recode.yn(rf_verbl),
                                frisked.bc.prior = recode.yn(rf_knowl),
                                frisked.bc.furtive = recode.yn(rf_furt),
                                frisked.bc.bulge = recode.yn(rf_bulg)) %>% 
        select(-rf_vcrim, -rf_othsw, -rf_attir, -rf_vcact, -rf_rfcmp, -rf_verbl, -rf_knowl, -rf_furt, -rf_bulg)

    print('STEP 9')
    # 9) secondary circumstances of stop
    sqf.data <- sqf.data %>% mutate(additional.report = recode.yn(ac_rept),
                                additional.investigation = recode.yn(ac_inves),
                                additional.proximity = recode.yn(ac_proxm),
                                additional.evasive = recode.yn(ac_evasv),
                                additional.associating = recode.yn(ac_assoc),
                                additional.direction = recode.yn(ac_cgdir),
                                additional.highcrime = recode.yn(ac_incid),
                                additional.time = recode.yn(ac_time),
                                additional.sights = recode.yn(ac_stsnd),
                                additional.other = recode.yn(ac_other)) %>% 
        select(-ac_rept, -ac_inves, -ac_proxm, -ac_evasv, -ac_assoc, -ac_cgdir, -ac_incid, -ac_time, -ac_stsnd, -ac_other)

    print('STEP 10')
    # 10) basis of search
    sqf.data <- sqf.data %>% mutate(searched.hardobject = recode.yn(sb_hdobj),
                                searched.outline = recode.yn(sb_outln),
                                searched.admission = recode.yn(sb_admis),
                                searched.other = recode.yn(sb_other)) %>% 
        select(-sb_hdobj, -sb_outln, -sb_admis, -sb_other)

    print('STEP 11')
    # 11) results of frisk/search
    sqf.data <- sqf.data %>% mutate(found.contraband = recode.yn(contrabn),
                                found.pistol = recode.yn(pistol),
                                found.rifle = recode.yn(riflshot),
                                found.assault = recode.yn(asltweap),
                                found.knife = recode.yn(knifcuti),
                                found.machinegun = recode.yn(machgun),
                                found.other = recode.yn(othrweap)) %>% 
        select(-contrabn, -pistol, -riflshot, -asltweap, -knifcuti, -machgun, -othrweap)

    print('STEP 12')
    # 12) demographics of stop subject

    # sex, race, and Hispanic/non-Hispanic
    sqf.data <- sqf.data %>% mutate(suspect.sex = recode.factor(sex, c('M', 'F'), c('male', 'female')),
                                suspect.race = recode.factor(race, c('A','B','I','P','Q','W','Z'), c('asian','black','native american','black hispanic','white hispanic','white','other')),
                                suspect.hispanic = (suspect.race %in% c('black hispanic','white hispanic'))) %>% 
        select(-sex, -race)

    # age and DOB
    sqf.data <- sqf.data %>% mutate(suspect.age = as.numeric(age), 
                                suspect.age = replace(suspect.age, suspect.age > 100, NA),
                                dob = sprintf("%08d", as.integer(dob)),
                                suspect.dob = mdy(dob),
                                suspect.dob = replace(suspect.dob, suspect.dob=='1900-12-31', NA)) %>% 
        select(-age, -dob)

    # height (in feet) and weight (in lbs)
    sqf.data <- sqf.data %>% mutate(suspect.height = (ht_feet + as.numeric(ht_inch)/12),
                                suspect.weight = weight,
                                suspect.weight = replace(suspect.weight, suspect.weight >= 700, NA)) %>% 
        select(-ht_feet, -ht_inch, -weight)

    # hair color, eye color, and build
    sqf.data <- sqf.data %>% mutate(suspect.hair = recode.factor(haircolr,
            c('BA','BK','BL','BR','DY','FR','GY', 'RD', 'SN', 'SP', 'WH', 'XX', 'ZZ'),
            c('bald', 'black', 'blond', 'brown', 'dyed', 'frosted', 'gray', 'red', 'sandy', 'salt and pepper', 'white', 'unknown', 'other')),
        suspect.eye = recode.factor(eyecolor,
            c('BK','BL','BR','GY','GR','HA', 'MA', 'Z', 'ZZ', 'P', 'PK','DF', 'XX',  'MC', 'VI'),
            c('black','blue','brown','gray','green','hazel', 'maroon',  'other', 'other','pink','pink', 'two different','unknown', 'unknown','violet')),
        suspect.build = recode.factor(build,
            c('H', 'M', 'T', 'U', 'Z'),
            c('heavy', 'medium', 'thin', 'muscular', 'unknown'))) %>% 
        select(-haircolr, -eyecolor, -build)

    print('STEP 13')
    # 13) add extra useful fields and filter data

    # fields for weapon found or gun found
    sqf.data <- sqf.data %>% mutate(found.gun = (found.pistol|found.rifle|found.assault|found.machinegun),
                                found.weapon = (found.pistol|found.rifle|found.assault|found.machinegun|found.knife|found.other))
    # add a unique id
    sqf.data$id <- 1:nrow(sqf.data)

    # eliminate all ages except for those between 10 and 80.
    sqf.data <- sqf.data %>% filter(suspect.age >= 10 & suspect.age <= 80)

    # convert coordinates to lat/lon
    coords <- proj4::project(list(sqf.data$xcoord, sqf.data$ycoord), nyc.proj, inverse=TRUE)
    sqf.data$lat <- coords$y
    sqf.data$lon <- coords$x

    print('STEP 14')
    # 14) final useful additions/changes
    # recode suspect.race for "white hispanic" and "black hispanic" to "hispanic"
    levels(sqf.data$suspect.race) <- c("asian", "black", "native.american", "hispanic", "hispanic", "white", "other")

    # add weekday, month, and time (6 four-hour-bins denoted by 1 through 6)
    sqf.data <- sqf.data %>% mutate(day = wday(timestamp, label = T, abbr = F),
        month = month(timestamp, label = T, abbr = F),
        time.period = case_when(
                                  hour(timestamp) < 4 ~ '1',
                                  hour(timestamp) >= 4 & hour(timestamp) < 8 ~ '2',
                                  hour(timestamp) >= 8 & hour(timestamp) < 12 ~ '3',
                                  hour(timestamp) >= 12 & hour(timestamp) < 16 ~ '4',
                                  hour(timestamp) >= 16 & hour(timestamp) < 20 ~ '5',
                                  hour(timestamp) >= 20 ~ '6',
                                ))

    # drop remaining irrelevant columns
    sqf.data <- sqf.data %>% select(-crimsusp, -repcmd, -revcmd, -othfeatr, -addrtyp,
        -rescode, -premtype, -premname, -addrnum, -stname,
        -stinter, -crossst, -aptnum, -state, -zip, -addrpct,
        -post, -serial)


    print('APPENDING...')
    # Append together
    if (year==2008) {
        sqf.full <- sqf.data
    } else {
        sqf.full <- plyr::rbind.fill(sqf.full, sqf.data)
    }
}

# Check no year has more than 10% missing in any variable except for:
# dob, beat, officer.verbal, officer.shield, arrested.reason
exceptions <- c('suspect.dob_nas', 'beat_nas', 'officer.verbal_nas', 'officer.shield_nas', 'arrested.reason')

sqf.full <- sqf.full %>% select(-forceuse)

proportion.nas <- sqf.full %>% group_by(year) %>% summarize_all(funs(nas=sum(is.na(.))/n()))
issues <- names(proportion.nas[apply(proportion.nas, 2, function(x) any(x > 0.10))])
issues <- issues[-which(issues %in% exceptions)]

# Write to csv in data/output directory
write_csv(sqf.full, "data/output/sqf_08_16.csv")
