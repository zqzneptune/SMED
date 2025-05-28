#' Merge Protein Complexes from Two Technique-Specific Lists
#'
#' Implements a comprehensive pipeline for merging protein complex predictions from
#' two different experimental techniques (e.g., AP-MS and Co-Frac). The merging
#' process follows these key steps:
#'
#' 1. **Input Preparation**: Converts raw lists to standardized S3 objects with
#'    metadata including technique ID and quality score.
#' 2. **Intra-List Cleaning**: Removes redundant complexes within each technique
#'    using specified similarity threshold and method.
#' 3. **Cross-Technique Merging**: Identifies and merges similar complexes across
#'    techniques using pairwise similarity comparisons.
#' 4. **Validation & Ranking**: Filters by size and applies scoring, with bonus
#'    for complexes supported by both techniques.
#'
#' **Biological Rationale**: 
#' - The merging approach accounts for technique-specific biases and coverage
#' - Similarity thresholds are based on empirical studies of complex conservation
#' - The scoring system favors complexes with cross-technique support
#'
#' **Algorithm Details**:
#' - Similarity metrics (Jaccard, Simpson, Dice) compare protein composition
#' - Greedy algorithm ensures each complex is merged at most once
#' - Validation includes size filtering and quality-based ranking
#'
#' **Performance Characteristics**:
#' - Time complexity: O(n*m) where n and m are complex counts in each list
#' - Memory usage: Moderate (stores similarity matrices during processing)
#' - Parallelization: Not currently implemented (future enhancement)
#'
#' @param raw_list1 A list where each element is a character vector of protein
#'   IDs, representing complexes from the first technique. Can be named; if not,
#'   default names will be generated (e.g., "Technique1_Cpx1").
#' @param raw_list2 A list similar to `raw_list1` for the second technique.
#' @param list1_id Character string identifier for `raw_list1` (e.g., "AP-MS").
#'   Used in naming and reporting. Default is "Technique1".
#' @param list2_id Character string identifier for `raw_list2`. Default "Technique2".
#' @param list1_quality_score Numeric (0-1) representing overall confidence in
#'   `raw_list1` (1=highest). Affects final complex scores. Default 1.0.
#' @param list2_quality_score Numeric (0-1) for `raw_list2`. Default 1.0.
#' @param redundancy_threshold Numeric (0-1), similarity threshold for removing
#'   redundant complexes *within* each list. Default 0.9 (very conservative).
#' @param similarity_method_intra Character, method for intra-list similarity:
#'   - "jaccard": Jaccard index (intersection/union) - default, balanced
#'   - "simpson": Simpson coefficient (intersection/smaller set) - sensitive
#'   - "dice": Dice coefficient (2*intersection/sum) - emphasizes overlap
#' @param merge_similarity_threshold Numeric (0-1), similarity threshold for
#'   merging complexes *between* techniques. Default 0.7 (moderate stringency).
#' @param similarity_method_inter Character, method for inter-list similarity.
#'   Same options as `similarity_method_intra`. Default "simpson".
#' @param validation_min_size Integer, minimum proteins for a valid complex.
#'   Default 2 (allowing dimers and larger).
#' @param validation_max_size Integer, maximum proteins. Default 50 (avoids
#'   non-specific aggregates).
#' @param validation_bonus_merged Numeric, multiplier for complexes supported by
#'   both techniques (reflects higher confidence). Default 1.5.
#' @param verbose Logical, if `TRUE` prints progress messages. Default `TRUE`.
#'
#' @return A list of `ProteinComplex` S3 objects (see \code{\link{ProteinComplex-S3}}),
#'   containing all validated complexes ranked by final score (highest first).
#'   Each complex includes:
#'   - proteins: Character vector of constituent proteins
#'   - original_name: Original complex name(s)
#'   - source_list_id: Technique ID(s)
#'   - quality_score: Combined quality metric
#'   - support_level: "both_techniques" or "[technique]_only"
#'   - merge_similarity: Similarity score (for merged complexes)
#'   - final_score: Composite score used for ranking
#'
#' @export
#' @seealso 
#' \code{\link{get_merged_complexes}} for extracting only merged complexes,
#' \code{\link{get_technique_specific_complexes}} for technique-specific ones,
#' \code{\link{get_all_complex_summary}} for comprehensive results analysis,
#' \code{\link{extract_all_protein_lists}} for protein-level outputs
#' @examples
#' # Basic example with small synthetic datasets
#' set.seed(123)
#' list1_data <- list(
#'   CpxA_t1 = c("P1", "P2", "P3", "P4"),
#'   CpxB_t1 = c("P3", "P4", "P5", "P6"),
#'   CpxOnly1 = c("P10", "P11")
#' )
#' list2_data <- list(
#'   CpxX_t2 = c("P1", "P2", "P3", "P5"), # Shares with CpxA_t1
#'   CpxY_t2 = c("P3", "P4", "P5", "P6", "P7", "P8"), # Shares with CpxB_t1
#'   CpxOnly2 = c("P20", "P21")
#' )
#'
#' results <- merge_protein_complexes(
#'   raw_list1 = list1_data, raw_list2 = list2_data,
#'   list1_id = "APMS", list2_id = "CoFrac",
#'   list1_quality_score = 0.9, list2_quality_score = 0.8,
#'   merge_similarity_threshold = 0.5, # Lowered for demo
#'   similarity_method_inter = "simpson",
#'   verbose = FALSE
#' )
#'
#' # Real-world usage with Havugimana datasets
#' \dontrun{
#' data(load_cofrac_HeLaCE12_tcs) # Technique 1: Co-Frac
#' data(load_pred_cpx_havugimana)  # Technique 2: AP-MS
#'
#' merged <- merge_protein_complexes(
#'   raw_list1 = load_cofrac_HeLaCE12_tcs(),
#'   raw_list2 = load_pred_cpx_havugimana(),
#'   list1_id = "CoFrac", list2_id = "APMS",
#'   list1_quality_score = 0.8, list2_quality_score = 0.7
#' )
#' }
merge_protein_complexes <- function(raw_list1, raw_list2,
                                  list1_id = "Technique1",
                                  list2_id = "Technique2",
                                  list1_quality_score = 1.0,
                                  list2_quality_score = 1.0,
                                  redundancy_threshold = 0.9,
                                  similarity_method_intra = "jaccard",
                                  merge_similarity_threshold = 0.7,
                                  similarity_method_inter = "simpson",
                                  validation_min_size = 2,
                                  validation_max_size = 50,
                                  validation_bonus_merged = 1.5,
                                  verbose = TRUE) {
  # [Implementation unchanged...]
}

#' Ensure List Elements Are Named
#'
#' Internal helper that ensures all elements in a protein complex list have names.
#' If names are missing, generates default names using the technique identifier
#' and sequential numbering.
#'
#' **Biological Significance**:
#' - Consistent naming is crucial for tracking complexes across analyses
#' - Technique-specific prefixes help maintain provenance information
#'
#' **Performance Characteristics**:
#' - Linear time complexity O(n) where n is number of complexes
#' - Minimal memory overhead
#'
#' @param raw_list A list of protein complexes (character vectors)
#' @param technique_id Identifier for the technique/dataset (used in naming)
#' @return A named list where each element has a name following the pattern
#'   "[technique_id]_Cpx[number]"
#' @details This function handles three cases:
#'   1. Completely unnamed list: Generates all names
#'   2. Partially named list: Fills in missing names
#'   3. Fully named list: Returns unchanged
#' @keywords internal
ensure_named_list <- function(raw_list, technique_id) {
  # [Implementation unchanged...]
}

#' Convert Raw List to ProteinComplex S3 Objects
#'
#' Internal helper that converts raw protein lists to structured S3 objects with
#' metadata including technique ID, quality score, and support level.
#'
#' **Biological Significance**:
#' - Standardized object format enables consistent processing
#' - Quality scores allow downstream weighting of evidence
#'
#' **Performance Characteristics**:
#' - Linear time complexity O(n)
#' - Memory usage proportional to input size
#'
#' @param named_list A named list of protein complexes (from ensure_named_list)
#' @param technique_id Identifier for the technique/dataset
#' @param quality_score Numeric quality score (0-1) for this dataset
#' @return A list of ProteinComplex S3 objects with these properties:
#'   - proteins: Character vector of protein IDs
#'   - original_name: Complex name from input list
#'   - source_list_id: Technique identifier
#'   - quality_score: Input quality score
#'   - support_level: Initialized as "[technique_id]_only"
#' @keywords internal
convert_raw_list_to_s3 <- function(named_list, technique_id, quality_score) {
  # [Implementation unchanged...]
}

#' Remove Redundant Complexes Within a Single List
#'
#' Internal helper that removes similar/redundant complexes from a single
#' technique-specific list using pairwise similarity comparisons.
#'
#' **Biological Significance**:
#' - Reduces over-representation of similar complexes
#' - Helps focus on distinct molecular entities
#'
#' **Performance Characteristics**:
#' - Quadratic time complexity O(n^2) due to pairwise comparisons
#' - Memory usage for similarity matrix O(n^2)
#' - Critical bottleneck for large input lists (>1000 complexes)
#'
#' @param s3_list List of ProteinComplex objects
#' @param threshold Similarity threshold (0-1) for considering redundancy
#' @param method Similarity method ("jaccard", "simpson", or "dice")
#' @return A filtered list of ProteinComplex objects where no two complexes
#'   have similarity >= threshold
#' @details The algorithm:
#'   1. Computes all pairwise similarities (upper triangular matrix)
#'   2. For each complex, marks similar ones (>= threshold) for removal
#'   3. Returns only non-redundant complexes
#' @keywords internal
remove_redundancy_intra_list <- function(s3_list, threshold, method) {
  # [Implementation unchanged...]
}

#' Calculate Similarity Between Two Complexes
#'
#' Internal helper that computes similarity between two protein complexes using
#' specified method (Jaccard, Simpson, or Dice).
#'
#' **Biological Significance**:
#' - Different metrics capture different aspects of complex similarity:
#'   - Jaccard: Balanced measure of overlap
#'   - Simpson: Sensitive to shared core components
#'   - Dice: Emphasizes common elements
#'
#' **Performance Characteristics**:
#' - Time complexity O(a + b) where a and b are complex sizes
#' - Memory efficient (only stores intersection/union sizes)
#'
#' @param complex1 First ProteinComplex object
#' @param complex2 Second ProteinComplex object
#' @param method Similarity method:
#'   - "jaccard": |A ∩ B| / |A ∪ B| (default for intra-list)
#'   - "simpson": |A ∩ B| / min(|A|, |B|) (default for inter-list)
#'   - "dice": 2|A ∩ B| / (|A| + |B|)
#' @return Numeric similarity score (0-1) where 1 = identical composition
#' @keywords internal
calculate_similarity <- function(complex1, complex2, method) {
  # [Implementation unchanged...]
}

#' Merge Two ProteinComplex Objects
#'
#' Internal helper that merges two complexes from different techniques, combining
#' their proteins and calculating a merged score based on similarity and quality
#' scores.
#'
#' **Biological Significance**:
#' - Represents consensus complexes supported by multiple techniques
#' - Combined quality scores reflect integrated confidence
#'
#' **Performance Characteristics**:
#' - Linear time O(a + b) for merging protein lists
#' - Minimal memory overhead
#'
#' @param complex1 First ProteinComplex object
#' @param complex2 Second ProteinComplex object
#' @param similarity_score Their precomputed similarity score
#' @return A merged ProteinComplex object with:
#'   - Combined protein list (unique union)
#'   - Concatenated original names and technique IDs
#'   - Average quality score
#'   - "both_techniques" support level
#'   - Initial score = avg_quality * similarity_score
#' @keywords internal
merge_two_complexes <- function(complex1, complex2, similarity_score) {
  # [Implementation unchanged...]
}

#' Validate and Rank Final Complexes
#'
#' Internal helper that filters complexes by size and applies scoring, then ranks
#' them by final score (highest first).
#'
#' **Biological Significance**:
#' - Size filters remove unrealistic complexes (too small/large)
#' - Scoring prioritizes high-confidence predictions
#'
#' **Performance Characteristics**:
#' - Linear time O(n) for filtering and scoring
#' - O(n log n) for ranking
#' - Memory efficient
#'
#' @param complexes List of ProteinComplex objects (merged and specific)
#' @param min_size Minimum allowed complex size (default 2)
#' @param max_size Maximum allowed complex size (default 50)
#' @param both_techniques_bonus Multiplier for complexes supported by both
#'   techniques (default 1.5)
#' @return A filtered and ranked list of ProteinComplex objects where:
#'   - All complexes pass size filters
#'   - Merged complexes have boosted scores
#'   - Ordered by descending final_score
#' @keywords internal
validate_and_rank_complexes <- function(complexes, min_size, max_size,
                                      both_techniques_bonus) {
  # [Implementation unchanged...]
}
