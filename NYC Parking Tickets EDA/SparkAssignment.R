# 1. Initialize spark session
# load SparkR
library(SparkR)

# initialise the spark session
sparkR.session(master='local')

# 2. Create a Spark DataFrame and examine structure
# reading a CSV file from S3 bucket
parking_violations_issued_2015 <- SparkR::read.df("s3://data-science-big-data-analytics-suresh/nyc-parking-case-study/Parking_Violations_Issued_-_Fiscal_Year_2015.csv", header=T, "CSV", na.strings = c("NA","NaN","","#DIV/0!"))
parking_violations_issued_2016 <- SparkR::read.df("s3://data-science-big-data-analytics-suresh/nyc-parking-case-study/Parking_Violations_Issued_-_Fiscal_Year_2016.csv", header=T, "CSV", na.strings = c("NA","NaN","","#DIV/0!"))
parking_violations_issued_2017 <- SparkR::read.df("s3://data-science-big-data-analytics-suresh/nyc-parking-case-study/Parking_Violations_Issued_-_Fiscal_Year_2017.csv", header=T, "CSV". na.strings = c("NA","NaN","","#DIV/0!"))

# examine the size
nrow(parking_violations_issued_2015)
# 11809233
ncol(parking_violations_issued_2015)
# 51

## Find out how many unique states the cars which got parking tickets came from.
collect(distinct(select(parking_violations_issued_2015, 'Registration State')))

## Some parking tickets don’t have addresses on them, which is cause for concern. Find out how many such tickets there are.
nrow(filter(parking_violations_issued_2015, isNull(parking_violations_issued_2015$`Street Name`) | isNull(parking_violations_issued_2015$`House Number`)))
## 1992401


## How often does each violation code occur? (frequency of violation codes - find the top 5)
voilationsByCode <- SparkR::sql("SELECT `Violation Code`, COUNT(*) FROM parking_violations_issued_2015_tbl GROUP BY `Violation Code` ORDER BY COUNT(*) DESC")
collect(voilationsByCode)

## How often does each vehicle body type get a parking ticket? How about the vehicle make? (find the top 5 for both)
voilationsByBodyType <- SparkR::sql("SELECT `Vehicle Body Type`, COUNT(*) FROM parking_violations_issued_2015_tbl GROUP BY `Vehicle Body Type` ORDER BY COUNT(*) DESC")
collect(voilationsByBodyType)

voilationsByMake <- SparkR::sql("SELECT `Vehicle Make`, COUNT(*) FROM parking_violations_issued_2015_tbl GROUP BY `Vehicle Make` ORDER BY COUNT(*) DESC")
collect(voilationsByMake)

## A precinct is a police station that has a certain zone of the city under its command. Find the (5 highest) frequencies of:
## Violating Precincts (this is the precinct of the zone where the violation occurred)
## Issuing Precincts (this is the precinct that issued the ticket)

voilationsByViolatingPrecincts <- SparkR::sql("SELECT `Violation Precinct`, COUNT(*) FROM parking_violations_issued_2015_tbl GROUP BY `Violation Precinct` ORDER BY COUNT(*) DESC")
collect(voilationsByViolatingPrecincts)

## 1                    0  1799170
## 2                   19   598351
## 3                   18   427510
## 4                   14   409064
## 5                    1   329009

voilationsByIssuerPrecincts <- SparkR::sql("SELECT `Issuer Precinct`, COUNT(*) FROM parking_violations_issued_2015_tbl GROUP BY `Issuer Precinct` ORDER BY COUNT(*) DESC")
collect(voilationsByIssuerPrecincts)

## 1                 0  2037745
## 2                19   579998
## 3                18   417329
## 4                14   392922
## 5                 1   318778

## Find the violation code frequency across 3 precincts which have issued the most number of tickets - do these precinct zones have an exceptionally high frequency of certain violation codes? Are these codes common across precincts?

df1 <- SparkR::sql(
  "SELECT `Violation Precinct`, `Violation Code`, COUNT(*)
  FROM parking_violations_issued_2015_tbl 
  WHERE `Violation Precinct` IN (SELECT `Violation Precinct` FROM parking_violations_issued_2015_tbl GROUP BY `Violation Precinct` ORDER BY COUNT(*) DESC LIMIT 3)
  GROUP BY `Violation Precinct`, `Violation Code`
  ORDER BY `Violation Precinct`, COUNT(*) DESC")
collect(df1)

1                    0             36   839197
2                    0              7   719746
3                    0              5   224517
4                    0             46     2386
5                    0             14     2059
6                    0             21     1817
7                    0             94     1684
8                    0             40     1200
9                    0             20     1131
10                   0             19      672
11                   0             71      494
12                   0             98      413
13                   0             78      410
14                   0             67      257
15                   0             17      245
16                   0             18      231
17                   0             74      228
18                   0             70      216
19                   0             38      210
20                   0             45      206
21                   0             69      158
22                   0             50      141
23                   0             51      140
24                   0             47      136
25                   0             16      124
26                   0             37      101
27                   0             41       97
28                   0             66       88
29                   0             31       80
30                   0             99       75
31                   0             48       66
32                   0             24       65
33                   0             27       53
34                   0             83       44
35                   0             80       43
36                   0             79       34
37                   0             85       29
38                   0              9       27
39                   0             62       25
40                   0             60       25
41                   0             42       25
42                   0             61       20
43                   0             10       19
44                   0             84       19
45                   0             64       19
46                   0             53       18
47                   0             77       17
48                   0             34       14
49                   0             11       14
50                   0             72       13
51                   0             91       11
52                   0             68       10
53                   0             75        8
54                   0             52        7
55                   0              1        7
56                   0              0        7
57                   0             73        7
58                   0             63        7
59                   0             13        6
60                   0             82        6
61                   0             97        6
62                   0              6        5
63                   0             95        4
64                   0              4        2
65                   0             54        2
66                   0             55        2
67                   0             96        2
68                   0             39        2
69                   0             49        2
70                   0             59        1
71                   0             22        1
72                   0             92        1
73                   0             35        1
74                   0             89        1
75                   0             65        1
76                   0             15        1
77                   0             88        1
78                   0              3        1
79                   0             30        1
80                   0             44        1
81                   0             56        1
82                   0             12        1
83                   0              2        1
84                   0              8        1
85                   0             76        1
86                   0             90        1
87                   0             58        1
88                   0             33        1
89                  18             14   131996
90                  18             69    60659
91                  18             31    33148
92                  18             47    31700
93                  18             42    21081
94                  18             38    20658
95                  18             46    19184
96                  18             84    10541
97                  18             19     9781
98                  18             37     9295
99                  18             20     9110
100                 18             40     8435
101                 18             16     8290
102                 18             13     7224
103                 18             71     5669
104                 18             17     5446
105                 18             82     4115
106                 18             21     4095
107                 18             10     2412
108                 18             70     2376
109                 18             35     2344
110                 18             45     1782
111                 18              8     1781
112                 18             48     1741
113                 18             74     1403
114                 18             64     1382
115                 18             51     1290
116                 18             50     1269
117                 18              9     1069
118                 18             77      966
119                 18             78      873
120                 18             53      788
121                 18             11      707
122                 18             66      555
123                 18             18      502
124                 18              1      495
125                 18             83      473
126                 18             98      353
127                 18             24      317
128                 18              3      303
129                 18             23      262
130                 18             73      258
131                 18             75      148
132                 18             41      137
133                 18             79      132
134                 18             89      126
135                 18             30      105
136                 18             60      102
137                 18             61       97
138                 18             72       92
139                 18             67       71
140                 18             26       50
141                 18             39       46
142                 18             85       34
143                 18             62       33
144                 18             32       29
145                 18             43       23
146                 18             99       21
147                 18             59       20
148                 18             81       13
149                 18              4       11
150                 18             49       11
151                 18             80       10
152                 18             52        9
153                 18             86        8
154                 18             22        8
155                 18              2        6
156                 18             12        5
157                 18             68        5
158                 18             34        4
159                 18             29        3
160                 18             15        2
161                 18             58        2
162                 18              7        2
163                 18             95        2
164                 18             25        2
165                 18             27        2
166                 18             55        2
167                 18             65        2
168                 18             54        1
169                 18             90        1
170                 18              5        1
171                 18             92        1
172                 18             97        1
173                 18             28        1
174                 18             57        1
175                 19             38    99836
176                 19             37    86013
177                 19             14    65315
178                 19             21    61821
179                 19             16    59943
180                 19             46    51529
181                 19             20    35608
182                 19             40    30120
183                 19             71    18521
184                 19             19    16328
185                 19             70     8651
186                 19             84     7221
187                 19             10     6581
188                 19             69     5097
189                 19             45     4961
190                 19             18     4716
191                 19             17     4101
192                 19             50     4094
193                 19             74     3117
194                 19             82     2910
195                 19             13     2723
196                 19             31     2465
197                 19             53     2062
198                 19             47     1857
199                 19             24     1599
200                 19             48     1594
201                 19             42     1419
202                 19             78     1340
203                 19             51     1207
204                 19             64     1022
205                 19             85      729
206                 19             11      453
207                 19             23      333
208                 19             73      291
209                 19             98      284
210                 19             61      279
211                 19             72      272
212                 19             49      197
213                 19             75      195
214                 19             83      185
215                 19             41      180
216                 19             67      172
217                 19             60      152
218                 19              9      142
219                 19             52      129
220                 19              8       83
221                 19             43       63
222                 19             59       57
223                 19             62       48
224                 19             39       41
225                 19             27       40
226                 19             79       34
227                 19             68       27
228                 19             81       23
229                 19             99       20
230                 19             77       18
231                 19             35       17
232                 19             66       16
233                 19             95       11
234                 19             80       11
235                 19             22       10
236                 19             44        9
237                 19             34        9
238                 19             12        6
239                 19             91        4
240                 19             32        4
241                 19             26        4
242                 19             30        3
243                 19             63        3
244                 19              1        3
245                 19              4        2
246                 19             15        2
247                 19             93        2
248                 19             86        2
249                 19             29        2
250                 19             56        2
251                 19             65        2
252                 19             87        1
253                 19             89        1
254                 19             55        1
255                 19             57        1
256                 19             97        1
257                 19              6        1
258                 19             28        1
259                 19             94        1
260                 19             33        1

head(filter(parking_violations_issued_2015, isNull(parking_violations_issued_2015$`Time First Observed`)))
nrow(filter(parking_violations_issued_2015, isNull(parking_violations_issued_2015$`Time First Observed`)))

df2 <- SparkR::sql(
  "SELECT *
  FROM parking_violations_issued_2015_tbl 
  WHERE `Time First Observed` IS NULL LIMIT 10"
)
collect(df2)


df2 <- SparkR::sql(
  "SELECT 
    SUM(IF(`Summons Number` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Summons Number`,
    SUM(IF(`Plate ID` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Plate ID`,
    SUM(IF(`Registration State` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Registration State`,
    SUM(IF(`Plate Type` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Plate Type`,
    SUM(IF(`Issue Date` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Issue Date`,
    SUM(IF(`Violation Code` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Violation Code`,
    SUM(IF(`Vehicle Body Type` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Vehicle Body Type`,
    SUM(IF(`Vehicle Make` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Vehicle Make`,
    SUM(IF(`Issuing Agency` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Issuing Agency`,
    SUM(IF(`Street Code1` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Street Code1`,
    SUM(IF(`Street Code2` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Street Code2`,
    SUM(IF(`Street Code3` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Street Code3`,
    SUM(IF(`Vehicle Expiration Date` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Vehicle Expiration Date`,
    SUM(IF(`Violation Location` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Violation Location`,
    SUM(IF(`Violation Precinct` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Violation Precinct`,
    SUM(IF(`Issuer Precinct` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Issuer Precinct`,
    SUM(IF(`Issuer Code` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Issuer Code`,
    SUM(IF(`Issuer Command` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Issuer Command`,
    SUM(IF(`Issuer Squad` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Issuer Squad`,
    SUM(IF(`Violation Time` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Violation Time`,
    SUM(IF(`Time First Observed` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Time First Observed`,
    SUM(IF(`Violation County` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Violation County`,
    SUM(IF(`Violation In Front Of Or Opposite` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Violation In Front Of Or Opposite`,
    SUM(IF(`House Number` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage House Number`,
    SUM(IF(`Street Name` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Street Name`,
    SUM(IF(`Intersecting Street` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Intersecting Street`,
    SUM(IF(`Date First Observed` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Date First Observed`,
    SUM(IF(`Law Section` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Law Section`,
    SUM(IF(`Sub Division` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Sub Division`,
    SUM(IF(`Violation Legal Code` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Violation Legal Code`,
    SUM(IF(`Days Parking In Effect    ` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Days Parking In Effect`,
    SUM(IF(`From Hours In Effect` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage From Hours In Effect`,
    SUM(IF(`To Hours In Effect` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage To Hours In Effect`,
    SUM(IF(`Vehicle Color` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Vehicle Color`,
    SUM(IF(`Unregistered Vehicle?` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Unregistered Vehicle?`,
    SUM(IF(`Vehicle Year` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Vehicle Year`,
    SUM(IF(`Meter Number` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Meter Number`,
    SUM(IF(`Feet From Curb` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Feet From Curb`,
    SUM(IF(`Violation Post Code` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Violation Post Code`,
    SUM(IF(`Violation Description` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Violation Description`,
    SUM(IF(`No Standing or Stopping Violation` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage No Standing or Stopping Violation`,
    SUM(IF(`Hydrant Violation` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Hydrant Violation`,
    SUM(IF(`Double Parking Violation` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Double Parking Violation`,
    SUM(IF(`Latitude` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Latitude`,
    SUM(IF(`Longitude` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Longitude`,
    SUM(IF(`Community Board` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Community Board`,
    SUM(IF(`Community Council ` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage`,
    SUM(IF(`Census Tract` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage Census Tract`,
    SUM(IF(`BIN` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage BIN`,
    SUM(IF(`BBL` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage BBL`,
    SUM(IF(`NTA` IS NULL, 1, 0))/COUNT(*) * 100 AS `NullsPercentage NTA`
  FROM parking_violations_issued_2015_tbl"
)
collect(df2)

df3 <- SparkR::sql(
  "SELECT `From Hours In Effect`, SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1) AS DayOrNight
  FROM parking_violations_issued_2015_tbl 
  LIMIT 10"
)
collect(df3)

df3 <- SparkR::sql(
  "SELECT `From Hours In Effect`, 
  SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1) AS DayOrNight, 
  IF(CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) > 0 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 12, 'Morning', 'Post Lunch') AS COM,
  SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`) - 2, 2) AS Minutes
  FROM parking_violations_issued_2015_tbl 
  LIMIT 10"
)
collect(df3)

--

df4 <- SparkR::sql(
  "SELECT COUNT(*) AS COUNT,
  CASE
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 0 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 4 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'A'
      THEN '0-4'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 4 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 8 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'A'
      THEN '4-8'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 8 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 12 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'A'
      THEN '8-12'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 0 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 4 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'P'
      THEN '12-16'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 4 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 8 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'P'
      THEN '16-20'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 8 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 12 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'P'
      THEN '20-24'
  END AS Time_Bin,
  `Violation Code`
  FROM parking_violations_issued_2015_tbl 
  GROUP BY
    Time_Bin, `Violation Code`
  ORDER BY Time_Bin, COUNT DESC"
)
collect(df4)

     COUNT Time_Bin Violation Code
1    839197     <NA>             36
2    719752     <NA>              7
3    651488     <NA>             14
4    595590     <NA>             46
5    576917     <NA>             71
6    534417     <NA>             40
7    343633     <NA>             19
8    311690     <NA>             20
9    252316     <NA>             70
10   224560     <NA>              5
11   115044     <NA>             74
12   106632     <NA>             50
13    90482     <NA>             21
14    79484     <NA>             51
15    77754     <NA>             48
16    71987     <NA>             84
17    63236     <NA>             17
18    53707     <NA>             98
19    39913     <NA>             67
20    39713     <NA>             45
21    39321     <NA>             10
22    35527     <NA>             82
23    35493     <NA>             85
24    34221     <NA>             53
25    33736     <NA>             13
26    32421     <NA>             78
27    31634     <NA>             16
28    29614     <NA>             66
29    18869     <NA>             77
30    17749     <NA>             68
31    17659     <NA>             64
32    15869     <NA>             83
33    15551     <NA>             72
34    15438     <NA>             61
35    14482     <NA>             38
36    13408     <NA>             60
37    11340     <NA>             27
38    11302     <NA>             24
39    10970     <NA>              9
40     8536     <NA>             80
41     7671     <NA>             99
42     7562     <NA>             62
43     6844     <NA>             18
44     6471     <NA>             75
45     6402     <NA>             69
46     6282     <NA>             47
47     6076     <NA>             37
48     5810     <NA>             73
49     5325     <NA>              1
50     4586     <NA>             23
51     4481     <NA>              3
52     4177     <NA>             41
53     4097     <NA>             11
54     3882     <NA>             31
55     3823     <NA>              8
56     2527     <NA>             79
57     2206     <NA>             91
58     2076     <NA>             42
59     1731     <NA>             49
60     1729     <NA>             94
61     1414     <NA>             52
62     1104     <NA>             63
63      774     <NA>             35
64      591     <NA>             59
65      552     <NA>             97
66      477     <NA>             30
67      472     <NA>             56
68      420     <NA>             55
69      324     <NA>             95
70      314     <NA>             39
71      259     <NA>              4
72      218     <NA>              6
73      169     <NA>             96
74      169     <NA>             25
75      137     <NA>             89
76      135     <NA>              2
77      129     <NA>             92
78      122     <NA>             81
79      104     <NA>             65
80       91     <NA>             86
81       89     <NA>             22
82       88     <NA>             26
83       79     <NA>             54
84       67     <NA>             34
85       66     <NA>             90
86       60     <NA>             88
87       58     <NA>             58
88       54     <NA>             43
89       53     <NA>             29
90       50     <NA>             76
91       38     <NA>             12
92       29     <NA>             93
93       27     <NA>             57
94       23     <NA>             15
95       17     <NA>              0
96       17     <NA>             44
97       14     <NA>             87
98       14     <NA>             28
99        9     <NA>             33
100       1     <NA>             32
101   20856      0-4             21
102    4167      0-4             14
103     779      0-4             20
104     452      0-4             38
105     238      0-4             37
106     171      0-4             16
107     169      0-4             18
108     163      0-4             17
109      78      0-4             41
110      55      0-4             19
111      46      0-4             78
112      40      0-4             68
113      39      0-4             69
114      26      0-4             24
115      24      0-4             31
116      16      0-4             47
117      12      0-4             71
118      11      0-4             85
119      11      0-4             46
120       7      0-4             42
121       4      0-4             40
122       4      0-4             10
123       3      0-4             74
124       3      0-4             13
125       2      0-4             70
126       2      0-4              4
127       1      0-4              8
128       1      0-4             67
129       1      0-4             39
130       1      0-4             98
131       1      0-4             45
132       1      0-4             48
133       1      0-4             63
134   13620    12-16             14
135    4789    12-16             18
136    2655    12-16             38
137    1042    12-16             37
138     337    12-16             16
139     273    12-16             20
140     260    12-16             17
141     113    12-16             69
142      84    12-16             19
143      39    12-16             31
144      23    12-16             42
145      18    12-16              4
146      13    12-16             78
147      12    12-16             21
148       5    12-16             10
149       3    12-16             24
150       3    12-16             41
151       2    12-16             68
152       2    12-16             15
153       2    12-16             35
154       2    12-16             48
155       2    12-16             79
156       1    12-16             46
157       1    12-16             63
158       1    12-16             12
159       1    12-16             74
160       1    12-16             85
161       1    12-16             98
162   68488    16-20             14
163   18050    16-20             38
164    7533    16-20             20
165    5392    16-20             37
166     958    16-20             17
167     954    16-20             18
168     639    16-20             16
169     319    16-20             26
170     160    16-20             21
171     154    16-20             13
172     149    16-20             69
173     102    16-20             24
174      84    16-20             35
175      76    16-20             31
176      55    16-20             78
177      50    16-20             19
178      44    16-20             47
179      40    16-20             42
180      29    16-20             10
181      26    16-20             23
182      22    16-20             41
183       4    16-20             46
184       3    16-20             48
185       3    16-20             43
186       3    16-20             34
187       2    16-20             15
188       2    16-20             79
189       2    16-20             89
190       2    16-20             85
191       2    16-20             12
192       2    16-20             32
193       1    16-20             27
194       1    16-20             98
195       1    16-20             83
196       1    16-20             50
197       1    16-20             51
198       1    16-20             11
199       1    16-20             74
200       1    16-20             45
201       1    16-20              8
202   46376    20-24             78
203   19990    20-24             14
204    2478    20-24             21
205    2179    20-24             38
206    1127    20-24             20
207    1055    20-24             37
208     321    20-24             13
209     135    20-24             16
210      98    20-24             63
211      58    20-24             41
212      55    20-24             10
213      43    20-24             31
214      42    20-24             17
215      32    20-24             69
216      27    20-24              6
217      19    20-24             85
218      17    20-24             39
219      15    20-24             42
220      14    20-24             24
221      13    20-24             27
222      13    20-24             18
223      10    20-24             26
224       9    20-24             83
225       9    20-24             46
226       9    20-24             19
227       5    20-24             74
228       5    20-24              4
229       4    20-24             23
230       3    20-24             98
231       3    20-24             68
232       2    20-24             40
233       2    20-24             75
234       1    20-24             70
235       1    20-24             35
236       1    20-24             84
237       1    20-24             66
238       1    20-24             53
239       1    20-24             79
240       1    20-24             96
241       1    20-24             55
242       1    20-24             82
243       1    20-24             67
244  199050      4-8             69
245  196406      4-8             14
246  134424      4-8             20
247  130654      4-8             16
248  130216      4-8             47
249  114727      4-8             21
250  113265      4-8             31
251   66940      4-8             42
252   47040      4-8             38
253   32519      4-8             17
254   28946      4-8             37
255   25228      4-8             24
256    6488      4-8             18
257    3622      4-8             35
258    3439      4-8             89
259    2032      4-8             23
260    1433      4-8             19
261     554      4-8             13
262     374      4-8             41
263     373      4-8             43
264     308      4-8             39
265     270      4-8              4
266     196      4-8             26
267      96      4-8             10
268      79      4-8             68
269      45      4-8             78
270      38      4-8             63
271      33      4-8             46
272      31      4-8             74
273      30      4-8             27
274      25      4-8             48
275      25      4-8             45
276       8      4-8             11
277       6      4-8             51
278       5      4-8             70
279       5      4-8             40
280       4      4-8             12
281       3      4-8             79
282       3      4-8             85
283       3      4-8             64
284       3      4-8             34
285       2      4-8             32
286       2      4-8             15
287       2      4-8             57
288       2      4-8             98
289       2      4-8             81
290       2      4-8             49
291       1      4-8             71
292       1      4-8             99
293       1      4-8             53
294       1      4-8             80
295       1      4-8             30
296       1      4-8             88
297       1      4-8             97
298       1      4-8             84
299       1      4-8             65
300       1      4-8              7
301       1      4-8             92
302       1      4-8              9
303       1      4-8             77
304       1      4-8             50
305       1      4-8             44
306       1      4-8             25
307 1402197     8-12             21
308 1333769     8-12             38
309  753169     8-12             37
310  207078     8-12             20
311   90806     8-12             16
312   79036     8-12             69
313   52522     8-12             31
314   34310     8-12             14
315   24345     8-12             42
316   10957     8-12             39
317   10881     8-12             17
318    3933     8-12             24
319    1328     8-12             41
320     857     8-12             35
321     755     8-12             23
322     557     8-12             19
323     480     8-12             13
324     402     8-12              4
325     372     8-12             32
326     327     8-12             78
327     198     8-12             43
328     107     8-12             74
329      98     8-12             47
330      61     8-12             46
331      47     8-12             10
332      26     8-12             27
333      21     8-12             68
 [ reached getOption("max.print") -- omitted 27 rows ]

--


df4 <- SparkR::sql(
  "SELECT `Violation Code`, COUNT(*) AS COUNT,
  CASE
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 0 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 4 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'A'
    THEN '0-4'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 4 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 8 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'A'
    THEN '4-8'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 8 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 12 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'A'
    THEN '8-12'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 0 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 4 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'P'
    THEN '12-16'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 4 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 8 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'P'
    THEN '16-20'
    WHEN CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) >= 8 AND CAST(SUBSTRING(`From Hours In Effect`, 0, 2) AS INT) < 12 AND SUBSTRING(`From Hours In Effect`, LENGTH(`From Hours In Effect`), 1)  == 'P'
    THEN '20-24'
  END AS Time_Bin,
  FROM parking_violations_issued_2015_tbl
  WHERE `Violation Code` IN (
    SELECT `Violation Code`
    FROM parking_violations_issued_2015_tbl
    GROUP BY `Violation Code`
    ORDER BY COUNT(*) DESC
    LIMIT 3
  )
  GROUP BY
  `Violation Code`, Time_Bin
  ORDER BY `Violation Code`, COUNT DESC"
)

collect(df4)

1   651488     <NA>             14
2   196406      4-8             14
3    68488    16-20             14
4    34310     8-12             14
5    19990    20-24             14
6    13620    12-16             14
7     4167      0-4             14
8  1402197     8-12             21
9   114727      4-8             21
10   90482     <NA>             21
11   20856      0-4             21
12    2478    20-24             21
13     160    16-20             21
14      12    12-16             21
15 1333769     8-12             38
16   47040      4-8             38
17   18050    16-20             38
18   14482     <NA>             38
19    2655    12-16             38
20    2179    20-24             38
21     452      0-4             38

---

df5 <- SparkR::sql(
  "SELECT
  CASE
    WHEN (CAST(SPLIT(`Issue Date`, '/')[0] AS INT) >= 1 AND CAST(SPLIT(`Issue Date`, '/')[0] AS INT) < 3) OR
          (CAST(SPLIT(`Issue Date`, '/')[0] AS INT) == 12)
      THEN 'Winter'
    WHEN CAST(SPLIT(`Issue Date`, '/')[0] AS INT) >= 3 AND CAST(SPLIT(`Issue Date`, '/')[0] AS INT) < 6
      THEN 'Spring'
    WHEN CAST(SPLIT(`Issue Date`, '/')[0] AS INT) >= 6 AND CAST(SPLIT(`Issue Date`, '/')[0] AS INT) < 9
      THEN 'Summer'
    WHEN CAST(SPLIT(`Issue Date`, '/')[0] AS INT) >= 9 AND CAST(SPLIT(`Issue Date`, '/')[0] AS INT) < 12
      THEN 'Fall'
  END AS Season,
  COUNT(*) AS COUNT
  FROM parking_violations_issued_2015_tbl
  GROUP BY Season
  ORDER BY COUNT"
)

collect(df5)

1   Fall 2794936
2 Winter 2899143
3 Spring 2956983
4 Summer 3158171

---

df6 <- SparkR::sql(
  "SELECT
  CASE
    WHEN (CAST(SPLIT(`Issue Date`, '/')[0] AS INT) >= 1 AND CAST(SPLIT(`Issue Date`, '/')[0] AS INT) < 3) OR
          (CAST(SPLIT(`Issue Date`, '/')[0] AS INT) == 12)
      THEN 'Winter'
    WHEN CAST(SPLIT(`Issue Date`, '/')[0] AS INT) >= 3 AND CAST(SPLIT(`Issue Date`, '/')[0] AS INT) < 6
      THEN 'Spring'
    WHEN CAST(SPLIT(`Issue Date`, '/')[0] AS INT) >= 6 AND CAST(SPLIT(`Issue Date`, '/')[0] AS INT) < 9
      THEN 'Summer'
    WHEN CAST(SPLIT(`Issue Date`, '/')[0] AS INT) >= 9 AND CAST(SPLIT(`Issue Date`, '/')[0] AS INT) < 12
      THEN 'Fall'
  END AS Season,
  `Violation Code`,
  COUNT(*) AS COUNT
  FROM parking_violations_issued_2015_tbl
  GROUP BY Season, `Violation Code` 
  ORDER BY Season, COUNT DESC"
  )

collect(df6)

1     Fall             21 360548
2     Fall             38 329955
3     Fall             36 244788
4     Fall             14 236540
5     Fall             37 191018
6     Fall             20 161899
7     Fall              7 151755
8     Fall             46 144437
9     Fall             71 134812
10    Fall             40 133702
11    Fall             19  82155
12    Fall             69  66754
13    Fall             16  61812
14    Fall             70  55824
15    Fall              5  41063
16    Fall             31  38907
17    Fall             47  32868
18    Fall             17  26478
19    Fall             50  24924
20    Fall             42  21963
21    Fall             74  21603
22    Fall             78  20640
23    Fall             48  19597
24    Fall             51  19239
25    Fall             84  17002
26    Fall             98  13297
27    Fall             67  10430
28    Fall             45  10007
29    Fall             24   9196
30    Fall             10   8975
31    Fall             13   8354
32    Fall             85   8298
33    Fall             82   8277
34    Fall             53   8000
35    Fall             66   7224
36    Fall             83   5407
37    Fall             77   4955
38    Fall             18   4668
39    Fall             68   4589
40    Fall             64   3979
41    Fall             61   3514
42    Fall             72   3249
43    Fall             39   3201
44    Fall              9   3066
45    Fall             27   2749
46    Fall             99   2438
47    Fall             80   2270
48    Fall             62   2107
49    Fall             60   1874
50    Fall             41   1452
51    Fall             75   1417
52    Fall             23   1318
53    Fall             73   1286
54    Fall             35   1191
55    Fall             89   1048
56    Fall             11    983
57    Fall              8    943
58    Fall             91    576
59    Fall             79    527
60    Fall             49    513
61    Fall             94    426
62    Fall             52    365
63    Fall              3    300
64    Fall              4    257
65    Fall             97    219
66    Fall             63    177
67    Fall             43    168
68    Fall             59    163
69    Fall              1    117
70    Fall             32    113
71    Fall             30    111
72    Fall             56    110
73    Fall             55    109
74    Fall             26     89
75    Fall              6     86
76    Fall             95     81
77    Fall              2     61
78    Fall             25     46
79    Fall             81     27
80    Fall             92     26
81    Fall             96     24
82    Fall             65     24
83    Fall             86     20
84    Fall             76     20
85    Fall             34     16
86    Fall             22     15
87    Fall             54     15
88    Fall             90     15
89    Fall             29     13
90    Fall             88     13
91    Fall             12     11
92    Fall             15      9
93    Fall             58      7
94    Fall             93      6
95    Fall             57      5
96    Fall             44      5
97    Fall             87      3
98    Fall              0      1
99    Fall             33      1
100   Fall             28      1
101 Spring             21 425354
102 Spring             38 327057
103 Spring             14 243771
104 Spring             36 226502
105 Spring             37 194261
106 Spring              7 165913
107 Spring             20 158176
108 Spring             46 152940
109 Spring             71 140644
110 Spring             40 130632
111 Spring              5  88308
112 Spring             19  82998
113 Spring             69  72850
114 Spring             70  67350
115 Spring             16  62513
116 Spring             31  41591
117 Spring             74  35951
118 Spring             47  35442
119 Spring             50  27124
120 Spring             17  25607
121 Spring             42  23423
122 Spring             51  19276
123 Spring             48  18606
124 Spring             84  18467
125 Spring             78  17121
126 Spring             98  11638
127 Spring             10  10540
128 Spring             24  10516
129 Spring             67   9589
130 Spring             82   9098
131 Spring             53   9087
132 Spring             13   8905
133 Spring             45   8774
134 Spring             85   8635
135 Spring             66   7103
136 Spring             72   5016
137 Spring             77   4414
138 Spring             18   4358
139 Spring             68   4220
140 Spring             64   4206
141 Spring             61   3827
142 Spring             60   2767
143 Spring             27   2507
144 Spring             83   2506
145 Spring             39   2491
146 Spring             23   2304
147 Spring              9   2138
148 Spring             80   1978
149 Spring             62   1831
150 Spring              1   1719
151 Spring             75   1664
152 Spring             73   1548
153 Spring             41   1466
154 Spring              3   1413
155 Spring             35   1190
156 Spring             99   1108
157 Spring             11   1053
158 Spring              8    804
159 Spring             89    737
160 Spring             79    631
161 Spring             91    450
162 Spring             94    357
163 Spring             52    320
164 Spring             49    317
165 Spring             63    307
166 Spring              4    226
167 Spring             26    217
168 Spring             43    158
169 Spring             59    139
170 Spring             55    133
171 Spring             30    103
172 Spring             56     88
173 Spring             32     69
174 Spring             97     49
175 Spring             95     43
176 Spring             92     38
177 Spring             81     34
178 Spring              6     29
179 Spring             96     28
180 Spring             25     25
181 Spring             58     23
182 Spring              2     21
183 Spring             22     21
184 Spring             65     18
185 Spring             12     17
186 Spring             29     14
187 Spring             86     12
188 Spring             44     12
189 Spring             54     11
190 Spring             90      9
191 Spring             33      8
192 Spring             93      7
193 Spring             34      7
194 Spring             76      5
195 Spring             88      4
196 Spring             28      3
197 Spring             15      1
198 Spring             57      1
199 Spring             87      1
200 Summer             21 476855
201 Summer             38 364578
202 Summer             14 257327
203 Summer              7 254134
204 Summer             37 206660
205 Summer             36 181524
206 Summer             20 168919
207 Summer             46 156778
208 Summer             71 149112
209 Summer             40 130981
210 Summer             19  90814
211 Summer             69  81434
212 Summer             70  68202
213 Summer             16  65725
214 Summer              5  54864
215 Summer             31  45437
216 Summer             47  37903
217 Summer             17  28689
218 Summer             74  28614
219 Summer             42  26898
220 Summer             50  24946
221 Summer             78  22495
222 Summer             48  20920
223 Summer             51  20834
224 Summer             84  18733
225 Summer             98  12856
226 Summer             24  11149
227 Summer             45  11132
228 Summer             67  10894
229 Summer             82  10814
230 Summer             10  10299
231 Summer             13   9670
232 Summer             85   9239
233 Summer             53   8575
234 Summer             66   7560
235 Summer             83   5342
236 Summer             18   5342
237 Summer             68   5189
238 Summer             64   5054
239 Summer             72   4767
240 Summer             77   4375
241 Summer              9   4337
242 Summer             61   4206
243 Summer             27   3301
244 Summer             39   3154
245 Summer             99   2544
246 Summer             80   2269
247 Summer             35   2253
248 Summer             62   2078
249 Summer             60   2072
250 Summer             75   1817
251 Summer             41   1816
252 Summer             23   1781
253 Summer             73   1630
254 Summer              8   1259
255 Summer             11   1123
256 Summer             89   1123
257 Summer             79    895
258 Summer             91    676
259 Summer             63    628
260 Summer             94    554
261 Summer             49    385
262 Summer             52    359
263 Summer              4    289
264 Summer             97    203
265 Summer             26    176
266 Summer              1    148
267 Summer             59    143
268 Summer             43    142
269 Summer             32    124
270 Summer             56    122
271 Summer             95    108
272 Summer             55    100
273 Summer              6     87
274 Summer             25     57
275 Summer             34     52
276 Summer             81     45
277 Summer             30     44
278 Summer             86     41
279 Summer             92     39
280 Summer             22     38
281 Summer             65     34
282 Summer             88     33
283 Summer              3     32
284 Summer             96     32
285 Summer             57     22
286 Summer             90     20
287 Summer             54     19
288 Summer              2     16
289 Summer             29     15
290 Summer             15     15
291 Summer             58     15
292 Summer             44     11
293 Summer              0     11
294 Summer             12     10
295 Summer             76     10
296 Summer             87      9
297 Summer             28      8
298 Summer             93      5
299 Summer             33      3
300 Winter             38 397037
301 Winter             21 368155
302 Winter             14 250831
303 Winter             37 203979
304 Winter             36 186383
305 Winter             20 173910
306 Winter             71 152372
307 Winter              7 147951
308 Winter             46 141554
309 Winter             40 139132
310 Winter             19  89854
311 Winter             16  64326
312 Winter             69  63783
313 Winter             70  60957
314 Winter             31  43916
315 Winter              5  40325
316 Winter             47  30443
317 Winter             50  29641
318 Winter             74  29024
319 Winter             17  27285
320 Winter             42  21162
321 Winter             51  20142
322 Winter             78  19027
323 Winter             48  18665
324 Winter             84  17788
325 Winter             98  15925
326 Winter             45   9827
327 Winter             24   9747
328 Winter             10   9743
329 Winter             85   9361
330 Winter             67   9003
331 Winter             53   8561
332 Winter             13   8319
333 Winter             66   7729


---

df7 <- SparkR::sql(
  "SELECT `Violation Code`,
  COUNT(*) AS COUNT
  FROM parking_violations_issued_2015_tbl
  GROUP BY `Violation Code`
  ORDER BY COUNT(*) DESC
  LIMIT 3"
)

collect(df7)

1             21 1630912
2             38 1418627
3             14  988469



# look at the first few rows
sample <- head(parking_violations_issued_2015)
View(sample)

# For using SQL, you need to create a temporary view
createOrReplaceTempView(parking_violations_issued_2015, "parking_violations_issued_2015_tbl")

sample_to_export <- SparkR::sql("SELECT * FROM parking_violations_issued_2015_tbl LIMIT 10000")
View(sample_to_export)
df <- collect(sample_to_export)
write.csv(df, "parking_violations_issued_2015_sample.csv")

##TODO: Check NA values and o
