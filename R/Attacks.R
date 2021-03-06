#' Data collected the day after the terrorist attacks in Brussels (on the morning of 22 March 2016) assessing the Sense of Coherence,
#' anxiety and depression symptoms of 2077 subjects (1056 were in Brussels during the terrorist attacks, and 1021 were not).
#'
#' The Sense of Coherence was assessed with the SOC-13 (Antonovsky, 1987): 7-point Likert scale (13 items)
#' Anxiety and depression were assessed with the HSCL-25 (Derogatis, Lipman, Rickels, Uhlenhuth & Covi, 1974).Subjects have to
#' mention in a 4-point Likert Scale how much there were bothered or upset by each trouble during the last 14 days
#' (1 = not at all; 2 = a little; quite a few; 4 = a lot).
#'
#' In french
#'
#' @name Attacks
#'
#' @docType data
#'
#' @usage data(Attacks)
#'
#' @format A data frame with 2077 rows and 46 variables:
#' \describe{
#'   \item{age}{age of participants, in years}
#'   \item{presencebxl}{were participants present in Brussels during the terrorist attacks; 1 = yes; -1 = no}
#'   \item{genre}{participant gender, 1 = female; -1 = male}
#'   \item{soc1}{Vous avez le sentiment que vous ne vous souciez pas reellement de ce qui se passe autour de vous: 1 = Tres rarement ou rarement; 7 = Souvent}
#'   \item{soc1r}{item1 reversed}
#'   \item{soc2}{Vous est-il arrive dans le passe d etre surpris(e) par le comportement de gens que vous pensiez connaitre tres bien ?: 1 = Jamais; 7 = Toujours}
#'   \item{soc2r}{item2 reversed}
#'   \item{soc3}{Est-il arrive que des gens sur lesquels vous comptiez vous decoivent ?: 1= Jamais; 7 = Toujours}
#'   \item{soc3r}{sense of coherence, item3 reversed}
#'   \item{soc4}{Jusqu a maintenant, votre vie : 1 = N a eu aucun but ni objectif clair; 7 = A eu des buts et des objectifs tres clairs}
#'   \item{soc5}{Avez-vous le sentiment que vous etes traite(e) injustement ?:1 = Tres souvent; 7 = Tres rarement ou jamais}
#'   \item{soc6}{Avez-vous le sentiment que vous etes dans une situation inconnue et que vous ne savez pas quoi faire ?: 1 = Tres souvent; 7 = Tres rarement ou jamais}
#'   \item{soc7}{Faire les choses que vous faites quotidiennement est : 1 = Une source de plaisir et de satisfaction; 7 = Une source de souffrance profonde et d ennui}
#'   \item{soc7r}{item7 reversed}
#'   \item{soc8}{Avez-vous des idees ou des sentiments confus(es) ?: 1 = Tres souvent; 7 =  Tres rarement ou jamais}
#'   \item{soc9}{Vous arrive-t-il d avoir des sentiments intimes que vous prefereriez ne pas avoir ?: 1 = Tres souvent; 7 = Tres rarement ou jamais}
#'   \item{soc10}{Beaucoup de gens (meme s’ils ont beaucoup de caractere) se sentent parfois de pauvres cloches.  Avez-vous deja eu ce sentiment dans le passe ?: 1 = Jamais; 7 = Tres souvent}
#'   \item{soc10r}{item10 reversed}
#'   \item{soc11}{Quand quelque chose arrive, vous trouvez generalement que : 1 = Vous surestimez ou sous-estimez son importance; 7 = Vous voyez les choses dans de justes proportions}
#'   \item{soc12}{Avez-vous le sentiment que les choses que vous faites dans la vie quotidienne  ont peu de sens ?: 1 = Tres souvent; 7 = Tres rarement ou jamais}
#'   \item{soc13}{Vous avez le sentiment que vous n etes pas sur(e) de vous maitriser : 1 = Tres souvent; 7 = Tres rarement ou jamais}
#'   \item{hsc1}{Mal de tete}
#'   \item{hsc2}{Tremblement}
#'   \item{hsc3}{Fatigue ou etourdissement}
#'   \item{hsc4}{Nervosite, agitation au fond de soi}
#'   \item{hsc5}{Peur soudaine sans raison particuliere}
#'   \item{hsc6}{Continuellement peureux ou anxieux}
#'   \item{hsc7}{Battements du coeur qui s'emballent}
#'   \item{hsc8}{Sensation d etre tendu, stresse}
#'   \item{hsc9}{Crise d angoisse ou de panique}
#'   \item{hsc10}{Tellement agite qu'il en est difficile de rester assis}
#'   \item{hsc11}{Manque d energie, tout va plus lentement que d habitude}
#'   \item{hsc12}{Se fait facilement des repproches}
#'   \item{hsc13}{Pleure facilement}
#'   \item{hsc14}{Pense a se tuer}
#'   \item{hsc15}{Mauvais appetit}
#'   \item{hsc16}{Probleme de sommeil}
#'   \item{hsc17}{Sentiment de desespoir en pensant au futur}
#'   \item{hsc18}{Decourage, morose}
#'   \item{hsc19}{Sentiment de solitude}
#'   \item{hsc20}{Perte d interets et d envies sexuelles}
#'   \item{hsc21}{Sentiment de s etre fait prendre au piège ou fait prisionnier}
#'   \item{hsc22}{Agite ou se tracasse beaucoup}
#'   \item{hsc23}{Aucun interet pour quoique ce soit}
#'   \item{hsc24}{Sentiment que tout est fatiguant}
#'   \item{hsc25}{Sentiment d etre inutile}
#' }
#'
#' @keywords Brussels Attacks Anxiety Depression Coherence
#'
#' @importFrom stats mahalanobis lm na.omit qchisq
"Attacks"
