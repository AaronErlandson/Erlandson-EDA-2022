
## Exploring Chronic Wasting Disease in Minnesota and the United States

Aaron Erlandson, Thomas Knutson

Minnesota State University Moorhead Biosciences Department

## Introduction

-   Chronic Wasting Disease

    -   Always fatal, contagious, or transmissible spongiform
        encephalopathy (TSE). Other TSE’s include Bovine sponigform
        encephalopathy (mad cow disease) in cattle and Creutzfeldt-Jakob
        disease in humans.

    -   Affects all members of the cervid (deer) family.

    -   Caused by missfolded prion protein.

    -   Spread through direct contact with bodily fluids, and indirectly
        through environmental contamination of soil, food, and water.

    -   Charectoriszed by emaciation, abnormal behavior, loss of bodily
        functions, and eventually death.

    -   Has an incubation of over a year, and symptoms develop slowly
        over time.

<img src="Poster picture 1.jpg" width="596"/>

-   First discovered in Colorado

    -   First positive case in captive cervids in 1967

    -   First positive wild case in wild cervids in 1981

    -   Since then, cases have been discovered in 29 different states
        across the country

-   Fatality rate

    -   CWD has a 100% positivity fatality, and there is no cure or
        vaccination for it.

    -   Monitoring is done to help track the spread of this disease and
        to help keep cervid populations safe.

-   Sample collection

    -   Testing lymph node samples is the most common way to test a
        cervid for potential CWD.

    -   Testing the brain is also used, but less common.

    -   Samples are generally acquired through agency culling, roadkill
        sampling, and hunter submitted samples.

-   Future risk of potential spread to humans

    -   Currently, CWD poses no threat to humans, as it cannot
        successfully infect us.

    -   However, there is a small possibility it could one day jump the
        species and infect humans sometime in the future.

    -   By studying CWD and tracking its spread, we will have a better
        understanding of the disease and how to combat it should it
        spread to humans.

## Methods

-   To collect our Minnesota data, we went to the Minnesota Department
    of Natural Resources website and used their public testing results.

    -   We gathered data for the total samples and total positive
        samples taken, the age and sex of the deer, the zone that the
        deer was taken in, and how the deer were harvested.

        -   Once we collected all our data, we gathered it into a Google
            Sheet, read it into RStudio and used R and RStudio to
            analyze our data and create our graphs.

-   To collect our country wide data, we went to each state’s Department
    of Natural Resources website, and used their public testing results.

    -   We gathered data for the total samples taken, total positive,
        the species of the deer, and the year and state the sample was
        taken in.

        -   Once we collected all of our data, we gathered it into a
            Google Sheet, combined all of the data from all of the
            analyzed states into one table, transferred it into RStudio,
            and used R and RStudio to analyze our data and create our
            graphs.

## Results

![](Prodject-Markdown-Document_files/figure-gfm/read%20data%20and%20Figure%201-1.png)<!-- -->

Figure 1: State totals for positive cases across state hunting permit
areas per year

![](Prodject-Markdown-Document_files/figure-gfm/Figure%202-1.png)<!-- -->

Figure 2: State totals for total collected samples, positive cases, and
positivity rare across the state per year

![](Prodject-Markdown-Document_files/figure-gfm/Figure%203-1.png)<!-- -->

Figure 3: State total positive case results based on the zone the sample
was collected in.

![](Prodject-Markdown-Document_files/figure-gfm/Figure%204-1.png)<!-- -->

Figure 4: State totals for positive cases by age per year

![](Prodject-Markdown-Document_files/figure-gfm/Figure%205-1.png)<!-- -->

Figure 5: State totals for total positive cases by sex per year.

![](Prodject-Markdown-Document_files/figure-gfm/Figure%206-1.png)<!-- -->

Figure 6: State totals for positive cases by sample acquisition per year

![](Prodject-Markdown-Document_files/figure-gfm/Figure%207-1.png)<!-- -->

Figure 7: Total samples collected across analyzed states per year

![](Prodject-Markdown-Document_files/figure-gfm/Figure%208-1.png)<!-- -->

Figure 8: Total positive cases for each state with a confirmed positive
case of CWD

![](Prodject-Markdown-Document_files/figure-gfm/Figure%209-1.png)<!-- -->

Figure 9. Total positive samples by species across analyzed states per
year.

![](Prodject-Markdown-Document_files/figure-gfm/Figure%2010-1.png)<!-- -->

Figure 10: Total positive cases across analyzed states per year

![](Prodject-Markdown-Document_files/figure-gfm/Figure%2011-1.png)<!-- -->

Figure 11: Total samples collected by species across analyzed states per
year

![](Prodject-Markdown-Document_files/figure-gfm/Figure%2012-1.png)<!-- -->

Figure 12: U.S. states containing infections in wild cervids

![](Prodject-Markdown-Document_files/figure-gfm/Figure%2013-1.png)<!-- -->

Figure 13: U.S. states containing infection in captive cervids

![](Prodject-Markdown-Document_files/figure-gfm/Figure%2014-1.png)<!-- -->

Figure 14: U.S. counties containing infections in wild cervids

![](Prodject-Markdown-Document_files/figure-gfm/Figure%2015-1.png)<!-- -->

Figure 15: U.S. counties containing infections in captive cervids

## Discussion

-   Landscape of CWD in the United States

    Based on our mapping of CWD cases across the United States, we found
    that the majority of positive cases were found in the Western and
    Midwestern states, with positive cases slowly appearing in the
    Southern and Eastern states.

    -   Our results also show that the amount of positive CWD cases is
        increasing over time.

    -   Testing should continue to be done in all states with confirmed
        cases in order to help track the spread of CWD.

<!-- -->

-   Data Acquisition

    -   Collecting data for each state was difficult at times. Many
        states had a lack of total sampling data and missing sampling
        year. Many states also presented their data in different ways,
        so it was difficult at times to format the data for each state
        the same way.

        -   We could not include Idaho in our data set due to a lack of
            provided data.

    -   Despite these limitations in the data, we were still able to
        paint a broad picture of the landscape of CWD in both Minnesota
        and the United States as a whole

-   Sample Acquisition

    -   While we were gathering our Minnesota positive case data, we
        discovered that the majority of the samples were obtained from
        samples collected from hunters.

        -   We also found this trend in the few states where sample
            acquisition methods was provided.

    -   Hunters submitting their collected samples are important to
        tracking the progress of CWD.

    -   Based on our data, state agencies could encourage hunters to
        submit samples from their harvested deer in order to increase
        the amount of samples available for testing.

-   How people can provide samples

    -   There are ways for people to provide voluntary samples for CWD
        testing.

        -   At a staffed sampling station

            -   Bring the deer to a staffed sampling station before it
                is frozen to make extraction of the lymph nodes easier

            -   Have your hunting license ready and know the location
                the deer was harvested in: township, range and section
                level.

        -   At self-service sampling station

            -   Once at the sampling station, follow the listed
                instructions to properly submit your sample

            -   Proper harvest information will be required otherwise
                the sample cannot be tested. This information includes
                township, range and section of harvest location.

            -   Videos are provided online on what to do and expect at a
                sampling station

    -   Sampling drop off locations are provided

-   How are states handling the spread of CWD?

    -   State measures for managing the spread of CWD include enforcing
        CWD requirements for wild and captive cervids, establishing
        state specific regulations, and conducting sampling of wild and
        captive cervids

## References

 Saunders SE, Bartelt-Hunt SL, Bartz JC. Occurrence, transmission, and
zoonotic potential of chronic wasting disease. 2012;18(3):369–376.
(Emerging infectious diseases).
<https://www.ncbi.nlm.nih.gov/pubmed/22377159.>
<doi:10.3201/eid1803.110685>

Richards, B.J., 2021, Chronic Wasting Disease distribution in the United
States by state and county: U.S. Geological Survey data release,
<https://doi.org/10.5066/P9HQKKFO>.

To see the list of state agencies we used to collect our national data
as well as a map of Minnesota permit zones, scan this QR to visit the
GitHub repository for this study where you can view this information.

-   Minnesota: [Chronic wasting disease management \| Minnesota DNR
    (state.mn.us)](https://www.dnr.state.mn.us/cwd/index.html)

-   Alabama: [CWD Information \| Outdoor
    Alabama](https://www.outdooralabama.com/CWD-Info)

-   Arkansas: [CWD Testing Options
    (agfc.com)](https://www.agfc.com/en/hunting/big-game/deer/cwd/cwd-test/)

-   Colorado: [Colorado Parks & Wildlife - Chronic Wasting Disease
    (state.co.us)](https://cpw.state.co.us/cwd)

-   Illinois: [Chronic Wasting Disease Management - Chronic Wasting
    Disease
    (illinois.gov)](https://www2.illinois.gov/dnr/programs/CWD/Pages/default.aspx)

-   Iowa: [Chronic Wasting Disease in Iowa \| Natural Resource
    Stewardship
    (iastate.edu)](https://naturalresources.extension.iastate.edu/wildlife/cwd)

-   Kansas: [Cervids - Chronic Wasting Disease (CWD)
    (ks.gov)](https://agriculture.ks.gov/divisions-programs/division-of-animal-health/animal-diseases/chronic-wasting-disease-(cwd))

-   Louisiana: [Chronic Wasting Disease \| Louisiana Department of
    Wildlife and Fisheries](https://www.wlf.louisiana.gov/page/cwd)

-   Maryland: [Chronic Wasting Disease (CWD) In
    Maryland](https://dnr.maryland.gov/wildlife/Pages/hunt_trap/CWD_in_Maryland.aspx)

-   Michigan: [Chronic Wasting Disease
    (michigan.gov)](https://www.michigan.gov/dnr/managing-resources/wildlife/cwd#:~:text=Chronic%20wasting%20disease%20has%20now,up%20with%20the%20latest%20news.)

-   Mississippi: [MDWFP - Chronic Wasting
    Disease](https://www.mdwfp.com/wildlife-hunting/chronic-wasting-disease/)

-   Missouri: [Chronic Wasting Disease \| Missouri Department of
    Conservation
    (mo.gov)](https://mdc.mo.gov/hunting-trapping/species/deer/chronic-wasting-disease#:~:text=Cards,CWD%20and%20limit%20its%20spread.)

-   Montana: [CWD Management \| Montana FWP
    (mt.gov)](https://fwp.mt.gov/cwd)

-   Nebraska: [Chronic Wasting Disease (CWD) - Nebraska Game and
    ParksNebraska Game and Parks \|
    (outdoornebraska.gov)](http://outdoornebraska.gov/cwd/)

-   New Mexico: [Chronic Wasting Disease - New Mexico Department of Game
    & Fish
    (state.nm.us)](https://www.wildlife.state.nm.us/conservation/invasive-species-and-diseases/chronic-wasting-disease/)

-   New York: [Chronic Wasting Disease - NYS Dept. of Environmental
    Conservation](https://www.dec.ny.gov/animals/7191.html)

-   North Dakota: [Chronic Wasting Disease \| North Dakota Game and
    Fish](https://gf.nd.gov/wildlife/diseases/cwd)

-   Ohio: [Chronic Wasting Disease (Deer) \| Ohio Department of Natural
    Resources
    (ohiodnr.gov)](https://ohiodnr.gov/buy-and-apply/hunting-fishing-boating/hunting-resources/chronic-wasting-disease)

-   Oklahoma: [Chronic Wasting Disease (CWD) \| Oklahoma Department of
    Wildlife Conservation
    (wildlifedepartment.com)](https://www.wildlifedepartment.com/hunting/species/deer/cwd)

-   Pennsylvania: [Chronic Wasting Disease
    (pa.gov)](https://www.pgc.pa.gov/Wildlife/Wildlife-RelatedDiseases/Pages/ChronicWastingDisease.aspx)

-   South Dakota: [Chronic Wasting Disease (CWD) \| South Dakota Game,
    Fish, and Parks
    (sd.gov)](https://gfp.sd.gov/chronic-wasting-disease/)

-   Tennessee: [Chronic Wasting Disease \| Tennessee State Wildlife
    Resources Agency (tn.gov)](https://www.tn.gov/twra/hunting/cwd.html)

-   Texas: [Chronic Wasting Disease
    (texas.gov)](https://tpwd.texas.gov/huntwild/wild/diseases/cwd/)

-   Utah: [Chronic wasting disease
    (utah.gov)](https://wildlife.utah.gov/chronic-wasting-disease.html)

-   Virginia: [Chronic Wasting Disease \| Virginia
    DWR](https://dwr.virginia.gov/wildlife/diseases/cwd/)

-   West Virginia: [CHRONIC WASTING DISEASE (CWD)
    (wv.gov)](https://oeps.wv.gov/cwd/pages/default.aspx)

-   Wisconsin:[Sampling for chronic wasting disease (CWD) \| \|
    Wisconsin
    DNR](https://dnr.wisconsin.gov/topic/WildlifeHabitat/registersample.html)

-   Wyoming: [Wyoming Game and Fish Department - Chronic Wasting Disease
    in Wyoming
    Wildlife](https://wgfd.wyo.gov/Wildlife-in-Wyoming/More-Wildlife/Wildlife-Disease/Chronic-Wasting-Disease)

## Acknowledgements

We would like to acknowledge and thank Dr. Chris Merkord for his help
and guidance on our project.
