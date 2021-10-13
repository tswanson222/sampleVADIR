#'Rank to pay grade data
#'
#'Serves as a key for relating certain military rank designations with pay
#'grades. Used in the \code{sampleVADIR()} function for stratifying based on pay
#'grade rather than rank.
#'
#'The way these data are used in the \code{sampleVADIR()} function is by
#'indexing the values of the \code{RANK_CD} variable of the VADIR dataset
#'against the \code{Initials} variable in the present dataset, and then the
#'\code{RANK_CD} value is replaced with the associated value in either the
#'\code{PayCat4} or \code{PayCat7} variable depending on what is specified in
#'the \code{sampleVADIR()} function. The purpose of this is to make the
#'\code{RANK_CD} variable more amenable to stratification, given the difficultly
#'of stratifying across values of a categorical variable with so many unique
#'values.
#'
#'@format A data frame with six variables that links pay grades to military
#'  ranks within each military branch. \code{PayGrade} indicates the pay grade
#'  associated with a specific job title (\code{Title}) within a given
#'  \code{Branch} of the military. \code{Title} designates the job title, where
#'  \code{Initials} is the shorthand for each title (this is how the
#'  \code{RANK_CD} variable is coded in the VADIR dataset). \code{Branch}
#'  designates the military branch, where \code{"N"} stands for Navy, \code{"A"}
#'  stands for Army, \code{"M"} stands for Marines, and \code{"F"} stands for
#'  Air Force. \code{PayCat4} represents one coding scheme that categorizes
#'  different pay grades into four categories, where \code{"E"} stands for
#'  enlisted, \code{"NCO"} stands for non-commissioned officer, \code{"W"}
#'  stands for warrant officer, and \code{"O"} stands for commissioned officer.
#'  \code{PayCat7} represents an alternative categorization that breaks pay
#'  grades into seven categories, wherein \code{"SNCO"} stands for senior
#'  non-commissioned officer, \code{"FGO"} stands for field grade officer,
#'  \code{"CGO"} stands for company grade officer, and \code{"GO"} stands for
#'  general officer.
"rankDat"

#'Fake VADIR data
#'
#'Simulated VADIR data based solely on the variable names and appropriate
#'response options for each. Does not represent population characteristics of
#'VADIR, and is meant simply as a faux tool for testing functions in the
#'\code{sampleVADIR} package.
#'
#'@format A data frame with ten variables, representing variables as they are
#'  formatted within the actual VADIR database.
"VADIR_fake"

