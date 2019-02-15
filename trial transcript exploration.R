d_transcript %>%
  count(`Student Number`, sort = TRUE)

d_transcript %>%
  count(`Module (Abbrev.)`, sort = TRUE)

d_transcript %>%
  count(`Module (Desc.)`, sort = TRUE)

d_transcript %>%
  count(`Program (Abbreviation)`, sort = TRUE) # looks like UCM is 7501

d_transcript %>%
  count(`Program (Description)`, sort = TRUE) # counts match

d_transcript %>%
  count(`Academic Year`) # 2007 - increase - 2009 - decrease - 2013 - increase - 2017

d_transcript %>%
  count(`Academic Session`, sort = TRUE) # decrease from 100 to 200 and from 400 to 500 caused by drop out?

d_transcript %>%
  count(`Appraisal Type`, sort = TRUE) # there are five main appraisal types (7055, 10, 7050, 7005, 7000)

d_transcript %>%
  count(`Appraisal (Description)`, sort = TRUE) # also five main appraisal description, but the counts do not match

d_transcript %>%
  count(`Number rep. attemp`)

x <- d_transcript %>%
  count(`Grade symbol`) # there are grades superior to 10

d_transcript %>%
  count(`Appraisal date`, sort = TRUE) # are these dates deadlines for grade submition?

d_transcript %>%
  count(`Attempted credits`, sort = TRUE) # mostly 0. Surprising.

d_transcript %>%
  count(`Earned Credits`, sort = TRUE) # mostly 0. Surprising.

d_transcript %>%
  count(`Individual Work`, sort = TRUE) # mostly NA

d_transcript %>%
  count(`Academic Work ID`, sort = TRUE) # unique id for each student-course combination

d_transcript %>%
  count(`Booking Status (Desc.)`) # what is booked?

d_transcript %>%
  count(`Object ID`, sort = TRUE) # moslty 0

d_transcript %>%
  count(`Appraiser ID`, sort = TRUE) 

d_transcript %>%
  count(`Appraiser Name`, sort = TRUE) # counts match




# Remark 1: course code are unique, course title are not
d_transcript %>%
  distinct(
    `Module (Abbrev.)`,
    `Module (Desc.)`
  ) %>%
  group_by(
    `Module (Desc.)`
  ) %>%
  summarise(
    abbrev = paste(`Module (Abbrev.)`, collapse = "; "),
    count = n()
  ) %>%
  arrange(desc(count))

# Remark 2: program code are unique, program desc are not
d_transcript %>%
  distinct(
    `Program (Abbreviation)`,
    `Program (Description)`
  ) %>%
  group_by(
    `Program (Description)`
  ) %>%
  summarise(
    abbrev = paste(`Program (Abbreviation)`, collapse = "; "),
    count = n()
  ) %>%
  arrange(desc(count))


