

| Name                 | Title                                                           | Description                                                                                                                                                                                                                                                                                                                                                                                                                         | Version | m.Date | m.Time | Author                                                                                                                                                     | Citation                                                                                                                                                                                                                                                                                                                                                              |
|:---------------------|:----------------------------------------------------------------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:--------|:-------|:-------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| create_ClusterSystem | Create dosimetric cluster system                                | In order to allow interaction of an spatial a correlation clusters in RLumCarlo, first a dosimetric system needs to be created in a three-dimensional space, which is the purpose of this function.                                                                                                                                                                                                                                 | 0.1.0   | NA     | NA     | Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom) -                                                               | Kreutzer, S., 2020. create_ClusterSystem(): Create dosimetric cluster system. Function version 0.1.0. In: Friedrich, J., Kreutzer, S., Pagonis, V., Schmidt, C., 2020. RLumCarlo: Monte-Carlo Methods for Simulating Luminescence Phenomena. R package version 0.1.7.9000-9. https://CRAN.R-project.org/package=RLumCarlo                                             |
| methods_RLumCarlo    | methods_RLumCarlo                                               | Methods for S3-generics implemented for the package 'RLumCarlo'.                                                                                                                                                                                                                                                                                                                                                                    | NA      | NA     | NA     | Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom), -  Johannes Friedrich, University of Bayreuth (Germany), -  | Kreutzer, S., Friedrich, J., 2020. methods_RLumCarlo(): methods_RLumCarlo. In: Friedrich, J., Kreutzer, S., Pagonis, V., Schmidt, C., 2020. RLumCarlo: Monte-Carlo Methods for Simulating Luminescence Phenomena. R package version 0.1.7.9000-9. https://CRAN.R-project.org/package=RLumCarlo                                                                        |
| plot_RLumCarlo       | Plot RLumCarlo Monte-Carlo Simulation Results                   | Visualise 'RLumCarlo' modelling results without extracting the values manually. Typically visualised are the averaged signal or the number of remaining electrons, with a polygon indicating modelling uncertainties.                                                                                                                                                                                                               | 0.1.0   | NA     | NA     | Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)  -  Johannes Friedrich, University of Bayreuth (Germany) -   | Kreutzer, S., Friedrich, J., 2020. plot_RLumCarlo(): Plot RLumCarlo Monte-Carlo Simulation Results. Function version 0.1.0. In: Friedrich, J., Kreutzer, S., Pagonis, V., Schmidt, C., 2020. RLumCarlo: Monte-Carlo Methods for Simulating Luminescence Phenomena. R package version 0.1.7.9000-9. https://CRAN.R-project.org/package=RLumCarlo                       |
| run_MC_CW_IRSL_LOC   | Monte-Carlo Simulation for CW-IRSL (localized transitions)      | Runs a Monte-Carlo (MC) simulation of continuous wave infrared stimulated luminescence (CW-IRSL) using the generalized one trap (GOT) model. Localized transitions refer to transitions which do not involve the conduction or valence band. These transitions take place between the ground state and an excited state of the trapped charge, and also involve an energy state of the recombination centre.                        | 0.1.0   | NA     | NA     | Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom) -                                                               | Kreutzer, S., 2020. run_MC_CW_IRSL_LOC(): Monte-Carlo Simulation for CW-IRSL (localized transitions). Function version 0.1.0. In: Friedrich, J., Kreutzer, S., Pagonis, V., Schmidt, C., 2020. RLumCarlo: Monte-Carlo Methods for Simulating Luminescence Phenomena. R package version 0.1.7.9000-9. https://CRAN.R-project.org/package=RLumCarlo                     |
| run_MC_CW_IRSL_TUN   | Run Monte-Carlo Simulation for CW-IRSL (tunnelling transitions) | Runs a Monte-Carlo (MC) simulation of continuous wave infrared stimulated luminescence (CW-IRSL) using the model for tunnelling transitions. Tunnelling refers to quantum mechanical tunnelling processes from the excited state of the trap, into a recombination centre.                                                                                                                                                          | 0.2.0   | NA     | NA     | Johannes Friedrich, University of Bayreuth (Germany), -  Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom) -   | Friedrich, J., Kreutzer, S., 2020. run_MC_CW_IRSL_TUN(): Run Monte-Carlo Simulation for CW-IRSL (tunnelling transitions). Function version 0.2.0. In: Friedrich, J., Kreutzer, S., Pagonis, V., Schmidt, C., 2020. RLumCarlo: Monte-Carlo Methods for Simulating Luminescence Phenomena. R package version 0.1.7.9000-9. https://CRAN.R-project.org/package=RLumCarlo |
| run_MC_CW_OSL_DELOC  | Run Monte-Carlo Simulation for CW-OSL (delocalized transitions) | Runs a Monte-Carlo (MC) simulation of continuous wave optically stimulated luminescence (CW-OSL) using the one trap one recombination centre (OTOR) model. The term delocalized here refers to the involvement of the conduction band.                                                                                                                                                                                              | 0.1.0   | NA     | NA     | Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom) -                                                               | Kreutzer, S., 2020. run_MC_CW_OSL_DELOC(): Run Monte-Carlo Simulation for CW-OSL (delocalized transitions). Function version 0.1.0. In: Friedrich, J., Kreutzer, S., Pagonis, V., Schmidt, C., 2020. RLumCarlo: Monte-Carlo Methods for Simulating Luminescence Phenomena. R package version 0.1.7.9000-9. https://CRAN.R-project.org/package=RLumCarlo               |
| run_MC_ISO_DELOC     | Run Monte-Carlo Simulation for ISO-TL (delocalized transitions) | Runs a Monte-Carlo (MC) simulation of isothermally stimulated luminescence (ISO-TL or ITL) using the one trap one recombination centre (OTOR) model. Delocalised refers to involvement of the conduction band.                                                                                                                                                                                                                      | 0.1.0   | NA     | NA     | Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom) -                                                               | Kreutzer, S., 2020. run_MC_ISO_DELOC(): Run Monte-Carlo Simulation for ISO-TL (delocalized transitions). Function version 0.1.0. In: Friedrich, J., Kreutzer, S., Pagonis, V., Schmidt, C., 2020. RLumCarlo: Monte-Carlo Methods for Simulating Luminescence Phenomena. R package version 0.1.7.9000-9. https://CRAN.R-project.org/package=RLumCarlo                  |
| run_MC_ISO_LOC       | Run Monte-Carlo simulation for ISO-TL (localized transitions)   | Runs a Monte-Carlo (MC) simulation of isothermally stimulated luminescence (ISO-TL or ITL) using the generalized one trap (GOT) model. Localized transitions refer to transitions which do no involve the conduction or valence band. These transitions take place between the ground state and an excited state of the trapped charge, and also involve an energy state of the recombination centre.                               | 0.1.0   | NA     | NA     | Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom) -                                                               | Kreutzer, S., 2020. run_MC_ISO_LOC(): Run Monte-Carlo simulation for ISO-TL (localized transitions). Function version 0.1.0. In: Friedrich, J., Kreutzer, S., Pagonis, V., Schmidt, C., 2020. RLumCarlo: Monte-Carlo Methods for Simulating Luminescence Phenomena. R package version 0.1.7.9000-9. https://CRAN.R-project.org/package=RLumCarlo                      |
| run_MC_ISO_TUN       | Monte-Carlo Simulation for ISO-TL (tunnelling transitions)      | Runs a Monte-Carlo (MC) simulation of isothermally stimulated luminescence (ISO-TL or ITL) using the tunnelling (TUN) model. Tunnelling refers to quantum mechanical tunnelling processes from the excited state of the trapped charge, into the recombination centre.                                                                                                                                                              | 0.1.0   | NA     | NA     | Johannes Friedrich, University of Bayreuth (Germany), -  Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom) -   | Friedrich, J., Kreutzer, S., 2020. run_MC_ISO_TUN(): Monte-Carlo Simulation for ISO-TL (tunnelling transitions). Function version 0.1.0. In: Friedrich, J., Kreutzer, S., Pagonis, V., Schmidt, C., 2020. RLumCarlo: Monte-Carlo Methods for Simulating Luminescence Phenomena. R package version 0.1.7.9000-9. https://CRAN.R-project.org/package=RLumCarlo          |
| run_MC_LM_OSL_DELOC  | Run Monte-Carlo Simulation for LM-OSL (delocalized transitions) | Runs a Monte-Carlo (MC) simulation of linearly modulated optically stimulated luminescence (LM-OSL) using the one trap one recombination centre (OTOR) model. Delocalised refers to involvement of the conduction band.                                                                                                                                                                                                             | 0.1.0   | NA     | NA     | Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom) -                                                               | Kreutzer, S., 2020. run_MC_LM_OSL_DELOC(): Run Monte-Carlo Simulation for LM-OSL (delocalized transitions). Function version 0.1.0. In: Friedrich, J., Kreutzer, S., Pagonis, V., Schmidt, C., 2020. RLumCarlo: Monte-Carlo Methods for Simulating Luminescence Phenomena. R package version 0.1.7.9000-9. https://CRAN.R-project.org/package=RLumCarlo               |
| run_MC_LM_OSL_LOC    | Run Monte-Carlo Simulation for LM-OSL (localized transitions)   | Runs a Monte-Carlo (MC) simulation of linearly modulated optically stimulated luminescence (LM-OSL) using the generalized one trap (GOT) model. Localized transitions refer to transitions which do not involve the conduction or valence band. These transitions take place between the ground state and an excited state of the trap, and also involve a an energy state of the recombination centre.                             | 0.1.0   | NA     | NA     | Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom) -                                                               | Kreutzer, S., 2020. run_MC_LM_OSL_LOC(): Run Monte-Carlo Simulation for LM-OSL (localized transitions). Function version 0.1.0. In: Friedrich, J., Kreutzer, S., Pagonis, V., Schmidt, C., 2020. RLumCarlo: Monte-Carlo Methods for Simulating Luminescence Phenomena. R package version 0.1.7.9000-9. https://CRAN.R-project.org/package=RLumCarlo                   |
| run_MC_LM_OSL_TUN    | Run Monte-Carlo Simulation for LM-OSL (tunnelling transitions)  | Runs a Monte-Carlo (MC) simulation of linearly modulated optically stimulated luminescence (LM-OSL) using the tunnelling (TUN) model. Tunnelling refers to quantum mechanical tunnelling processes from the excited state of the trapped charge, into a recombination centre.                                                                                                                                                       | 0.1.0   | NA     | NA     | Johannes Friedrich, University of Bayreuth (Germany), -  Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom) -   | Friedrich, J., Kreutzer, S., 2020. run_MC_LM_OSL_TUN(): Run Monte-Carlo Simulation for LM-OSL (tunnelling transitions). Function version 0.1.0. In: Friedrich, J., Kreutzer, S., Pagonis, V., Schmidt, C., 2020. RLumCarlo: Monte-Carlo Methods for Simulating Luminescence Phenomena. R package version 0.1.7.9000-9. https://CRAN.R-project.org/package=RLumCarlo   |
| run_MC_TL_DELOC      | Run Monte-Carlo Simulation for TL (delocalized transitions)     | Runs a Monte-Carlo (MC) simulation of thermoluminescence (TL) using the one trap one recombination centre (OTOR) model. Delocalised refers to involvement of the conduction band. The heating rate in this function is assumed to be 1 K/s.                                                                                                                                                                                         | 0.1.0   | NA     | NA     | Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom) -                                                               | Kreutzer, S., 2020. run_MC_TL_DELOC(): Run Monte-Carlo Simulation for TL (delocalized transitions). Function version 0.1.0. In: Friedrich, J., Kreutzer, S., Pagonis, V., Schmidt, C., 2020. RLumCarlo: Monte-Carlo Methods for Simulating Luminescence Phenomena. R package version 0.1.7.9000-9. https://CRAN.R-project.org/package=RLumCarlo                       |
| run_MC_TL_LOC        | Run Monte-Carlo Simulation for TL (localized transitions)       | Runs a Monte-Carlo (MC) simulation of thermoluminescence (TL) using the generalized one trap (GOT) model. Localized transitions refer to transitions which do not involve the conduction or valence band. These transitions take place between the ground state and an excited state of the trapped charge, and also involve an energy state of the recombination centre. The heating rate in this function is assumed to be 1 K/s. | 0.1.0   | NA     | NA     | Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom) -                                                               | Kreutzer, S., 2020. run_MC_TL_LOC(): Run Monte-Carlo Simulation for TL (localized transitions). Function version 0.1.0. In: Friedrich, J., Kreutzer, S., Pagonis, V., Schmidt, C., 2020. RLumCarlo: Monte-Carlo Methods for Simulating Luminescence Phenomena. R package version 0.1.7.9000-9. https://CRAN.R-project.org/package=RLumCarlo                           |
| run_MC_TL_TUN        | Run Monte-Carlo Simulation for TL (tunnelling transitions)      | Runs a Monte-Carlo (MC) simulation of thermoluminescence (TL) caused by tunnelling (TUN) transitions. Tunnelling refers to quantum mechanical tunnelling processes from the excited state of the trap into a recombination centre. The heating rate in this function is assumed to be 1 K/s.                                                                                                                                        | 0.1.0   | NA     | NA     | Johannes Friedrich, University of Bayreuth (Germany), Sebastian Kreutzer, -  Geography & Earth Sciences, Aberystwyth University (United Kingdom) -   | Friedrich, J., Kreutzer, S., 2020. run_MC_TL_TUN(): Run Monte-Carlo Simulation for TL (tunnelling transitions). Function version 0.1.0. In: Friedrich, J., Kreutzer, S., Pagonis, V., Schmidt, C., 2020. RLumCarlo: Monte-Carlo Methods for Simulating Luminescence Phenomena. R package version 0.1.7.9000-9. https://CRAN.R-project.org/package=RLumCarlo           |
