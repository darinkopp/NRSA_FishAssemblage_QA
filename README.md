National Rivers and Stream Assessment Fish Assemblage Quality Assurance
Procedure 2023-2024 (Draft)
================
Darin Kopp, Richard Mitchell, Louis Reynolds, and Dave Peck
3-31-2025

Background: Fish assemblages are a key indicator of biological integrity
and water quality surveyed by the National Rivers and Streams
Assessment. Technical Reports and Field and Laboratory Manuals provide
critical details about how these data are collected. This document and
associated scripts outline the QA/QC procedure used to ensure the data
reported by field crews are of the highest quality for use in biological
assessment. This is a semi-automated procedure with three modules that
efficiently identifies and corrects inconsistencies or errors in the
data (Figure 1).

![Schematic of quality assurance procedure for fish
assemblages](Picture1.png) Name Reconciliation: The name reconciliation
module harmonizes field identifications with the NRSA taxa list. Any
identifications that were unknown or not easily reconciled were sent to
the field crew for comment. If the field crew indicated that updates
were made, corrections are added to NARS IM. Any taxa that were not
collected during a previous survey are added to the NRSA taxa list along
with their autecology information.

Sampling sufficiency: The sampling sufficiency module ensures that
sampling efforts were consistent with the protocol outlined in the Field
Manual such that the sampled assemblage is representative of the entire
community. The categories used are consistent with previous surveys and
balances input from field crew, total reach length, reach length fished
and number of individuals collected. Instances where there was
disagreement between the assignment and the value (Y/N) reported by the
crews were checked manually.

Range and Nativeness Checks: The range and nativeness checks determine
whether a specimen identified in the field was consistent with its known
range and assigned native/nonnative status for each species at the HUC8
level. This module first created a table from NAS, NATURESERVE and
previous NRSA surveys containing all species occurrences and HUC8
combinations and the native/nonnative status. The script then
iteratively compared each 2324 occurrence to the table leveraging nested
hydrologic unit codes. Any 2324 occurrences that could not be matched to
the nativeness table were manually checked using all available
resources.
