Figures for conference paper
================
DARS
2019-06-03

-   [Set up](#set-up)
-   [Student data](#student-data)
-   [Course data](#course-data)
-   [Flow chart](#flow-chart)
-   [Model Selection (topic model)](#model-selection-topic-model)
-   [Beta](#beta)
-   [Gamma](#gamma)
-   [Topic Expertise](#topic-expertise)
-   [CV](#cv)

Set up
======

    ## # A tibble: 10 x 5
    ##    `Student ID` `Course ID` `Academic Year` Period Grade
    ##    <chr>        <chr>       <chr>           <chr>  <dbl>
    ##  1 44940        CAP3000     2009-2010       4        8.8
    ##  2 37490        SSC2037     2009-2010       4        8.4
    ##  3 71216        HUM1003     2010-2011       4        6.8
    ##  4 44212        SSC2049     2010-2011       2        8.4
    ##  5 85930        SSC2043     2011-2012       1        4.3
    ##  6 14492        COR1004     2012-2013       2        8.5
    ##  7 34750        HUM2049     2013-2014       5        6  
    ##  8 32316        SSC1001     2013-2014       1        8.5
    ##  9 22092        SCI1009     2014-2015       1        6.4
    ## 10 19512        COR1004     2016-2017       5        7

    ## # A tibble: 10 x 4
    ##    `Course ID` `Course Title` Department word      
    ##    <chr>       <chr>          <chr>      <chr>     
    ##  1 HUM3034     World History  UCM        understand
    ##  2 HUM3034     World History  UCM        major     
    ##  3 HUM3034     World History  UCM        issue     
    ##  4 HUM3034     World History  UCM        episode   
    ##  5 HUM3034     World History  UCM        shape     
    ##  6 HUM3034     World History  UCM        history   
    ##  7 HUM3034     World History  UCM        mankind   
    ##  8 HUM3034     World History  UCM        focus     
    ##  9 HUM3034     World History  UCM        theme     
    ## 10 HUM3034     World History  UCM        topic

Student data
============

![](Figures/student_data.pdf)

Course data
===========

![](Figures/course_data.pdf)

Flow chart
==========

![](Figures/unnamed-chunk-2-1.png)

Model Selection (topic model)
=============================

![](Figures/unnamed-chunk-3-1.png)

Beta
====

Gamma
=====

Topic Expertise
===============

![](Figures/toy_transcript.pdf)![](Figures/toy_gamma.pdf)![](Figures/toy_topic_expertise.pdf)![](Figures/unnamed-chunk-4-4.png)

CV
==

![](Figures/unnamed-chunk-5-1.png)![](Figures/CV.pdf)
