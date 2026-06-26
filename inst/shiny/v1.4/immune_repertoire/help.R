## ---- Help text formatter ---------------------------------------------- ##
.format_detail <- function(txt) {
  lines <- strsplit(txt, "\n")[[1]]
  out <- list()
  ul_buf <- character(0)
  is_first_para <- TRUE

  ## -- inline markup: 'term' -> bold accent; em-dash split in bullets ----
  .inline <- function(s) {
    # Replace 'quoted' terms with styled <b>
    s <- gsub("'([^']+)'", "<b style='color:#0f6cbd;'>\\1</b>", s)
    HTML(s)
  }

  .make_li <- function(raw) {
    txt <- sub("^\\s*\u2022\\s*", "", raw)
    # Split on em-dash: bold the key part, normal the explanation
    if (grepl("\u2014", txt)) {
      parts <- strsplit(txt, "\\s*\u2014\\s*", perl = TRUE)[[1]]
      tagList(
        tags$li(
          style = "margin: 4px 0; line-height: 1.5;",
          tags$strong(.inline(parts[1])),
          if (length(parts) > 1) {
            tagList(
              " \u2014 ",
              tags$span(
                style = "color: var(--neutral-secondary);",
                .inline(paste(parts[-1], collapse = " \u2014 "))
              )
            )
          }
        )
      )
    } else {
      tags$li(style = "margin: 4px 0; line-height: 1.5;", .inline(txt))
    }
  }

  flush_ul <- function() {
    if (length(ul_buf) > 0) {
      items <- lapply(ul_buf, .make_li)
      out[[length(out) + 1L]] <<- do.call(
        tags$ul,
        c(
          items,
          list(
            style = "padding-left: 22px; margin: 8px 0; list-style-type: disc;"
          )
        )
      )
      ul_buf <<- character(0)
    }
  }

  for (ln in lines) {
    if (grepl("^\\s*\u2022", ln)) {
      ## bullet line
      ul_buf <- c(ul_buf, ln)
    } else if (trimws(ln) == "") {
      ## blank line -> flush
      flush_ul()
    } else if (grepl(":$", trimws(ln))) {
      ## section header (e.g. "What to look for:")
      flush_ul()
      out[[length(out) + 1L]] <- tags$p(
        style = "margin: 12px 0 4px 0; font-weight: 600; color: #0f6cbd; font-size: 14px;",
        sub(":$", "", trimws(ln))
      )
    } else {
      ## regular paragraph
      flush_ul()
      if (is_first_para) {
        out[[length(out) + 1L]] <- tags$p(
          style = "margin: 6px 0; line-height: 1.6; font-size: 14px;",
          .inline(ln)
        )
        is_first_para <- FALSE
      } else {
        out[[length(out) + 1L]] <- tags$p(
          style = "margin: 6px 0; line-height: 1.6; color: var(--neutral-primary);",
          .inline(ln)
        )
      }
    }
  }
  flush_ul()
  do.call(tagList, out)
}

## ---- Help text for each visualization tab ----------------------------- ##
ir_tab_help <- list(
  Abundance = list(
    short = "Clonal abundance distribution",
    summary = "Ranks clonotypes by cell count. Steep drop-off indicates oligoclonal dominance; gradual decline indicates diverse repertoire.",
    detail = paste(
      "Every T or B cell carries a unique receptor sequence (called a 'clonotype').",
      "Some clonotypes are found in many cells (expanded clones), while most appear only once or twice (rare clones).",
      "",
      "This plot ranks all clonotypes by how many cells carry them.",
      "The X-axis is the rank (1 = most common clone), and the Y-axis is the number of cells.",
      "",
      "What to look for:",
      "\u2022 A steep drop-off means a few clones dominate \u2014 this often happens after infection or in tumors where certain T/B cells multiply rapidly.",
      "\u2022 A flat, gradual curve means many clones are roughly equal in size \u2014 typical of a resting, diverse immune system.",
      "\u2022 Compare samples: if one sample has a much steeper curve, that sample likely experienced stronger clonal expansion.",
      sep = "\n"
    )
  ),
  Diversity = list(
    short = "Repertoire diversity",
    summary = "Quantifies clonotype richness and evenness using Shannon entropy. Higher values reflect broader, more balanced repertoires.",
    detail = paste(
      "Diversity measures how many different clonotypes exist AND how evenly they are distributed.",
      "Think of it like species diversity in an ecosystem \u2014 a forest with 100 equally common tree species is more 'diverse' than one with 100 species where a single species makes up 99%.",
      "",
      "How the plot works — bootstrap resampling:",
      "\u2022 For each sample, scRepertoire randomly draws clonotypes (with replacement) to create a 'resampled' dataset of the same size, then calculates Shannon entropy. This process is repeated many times (controlled by 'Bootstrap iterations').",
      "\u2022 The result is a distribution of diversity values — not a single number, but a range that reflects how stable the estimate is given the number of clonotypes in that sample.",
      "",
      "What the dots (jitter points) actually represent:",
      "\u2022 Each jitter point is ONE bootstrap replicate \u2014 one diversity value computed from one random resampling of the original clonotype pool. They are NOT independent biological observations.",
      "\u2022 The boxplot summarises the bootstrap distribution: median line is the point estimate, box is the middle 50% (IQR), whiskers show the range.",
      "\u2022 A narrow box means the diversity estimate is stable (bootstrap repeatedly gives similar values). A wide box means the estimate is more uncertain \u2014 typical for samples with few clonotypes.",
      "",
      "When 'Group by' and 'X axis' are the SAME column (e.g. both set to 'sample'):",
      "\u2022 Each x-axis position has exactly one group. All jitter points at that position belong to that single group \u2014 they show the bootstrap uncertainty of that group's diversity estimate.",
      "\u2022 This is correct behaviour, not a bug: the jitter is visualising the spread of bootstrap replicates, not displaying multiple independent measurements.",
      "",
      "When 'Group by' and 'X axis' are DIFFERENT (e.g. Group by = cell_type, X axis = condition):",
      "\u2022 Each x-axis position contains multiple groups side-by-side.",
      "\u2022 Jitter points are colour-coded by Group by, making it easy to compare diversity across categories at each condition.",
      "",
      "What to look for:",
      "\u2022 Higher values = more diverse repertoire (many clonotypes, evenly distributed).",
      "\u2022 Lower values = less diverse (dominated by a few expanded clones).",
      "\u2022 After vaccination or infection, diversity often drops temporarily as specific clones expand.",
      "\u2022 In autoimmune diseases, you may see persistently low diversity in the affected tissue.",
      "\u2022 Bootstrapping helps you judge whether differences between samples are meaningful or just random variation \u2014 non-overlapping boxplots suggest a genuine difference.",
      sep = "\n"
    )
  ),
  Homeostasis = list(
    short = "Clonal homeostasis",
    summary = "Categorises clonotypes into size classes (Rare to Hyperexpanded). Shifts toward larger classes indicate active clonal expansion.",
    detail = paste(
      "This plot groups all clonotypes into size categories based on how many cells carry them:",
      "\u2022 Rare: appears in very few cells",
      "\u2022 Small: slightly more common",
      "\u2022 Medium: moderately expanded",
      "\u2022 Large: substantially expanded",
      "\u2022 Hyperexpanded: found in a very large number of cells",
      "",
      "Each bar shows the proportion of cells belonging to each category for a given sample.",
      "",
      "What to look for:",
      "\u2022 A healthy resting repertoire is mostly 'Rare' and 'Small' clones.",
      "\u2022 After immune activation (infection, vaccination), you'll see more cells shifting into 'Large' and 'Hyperexpanded' categories.",
      "\u2022 Comparing samples side-by-side reveals which conditions drive more clonal expansion.",
      "\u2022 In cancer, tumor-infiltrating lymphocytes often show a high proportion of hyperexpanded clones (indicating anti-tumor response or exhaustion).",
      sep = "\n"
    )
  ),
  Length = list(
    short = "CDR3 length distribution",
    summary = "Distribution of CDR3 region lengths. Shifts in the peak may indicate antigen-driven selection constraining receptor structure.",
    detail = paste(
      "The CDR3 region is the most variable part of a T/B cell receptor \u2014 it's the primary 'fingers' that grab onto antigens (foreign molecules).",
      "CDR3 length directly affects what a receptor can bind to.",
      "",
      "This plot shows how many clonotypes have each possible CDR3 length (in amino acids or nucleotides).",
      "",
      "What to look for:",
      "\u2022 TCR CDR3 lengths typically peak around 12\u201315 amino acids; BCR CDR3 lengths have a wider range.",
      "\u2022 A shift in the peak length between conditions may indicate selection for receptors that bind a specific antigen shape.",
      "\u2022 Very long or very short CDR3 are often self-reactive and may be removed by the immune system (negative selection).",
      "\u2022 If one sample has a narrower length distribution, it suggests selection pressure is constraining which receptors survive.",
      sep = "\n"
    )
  ),
  Proportion = list(
    short = "Clonal proportion",
    summary = "Cumulative fraction of the repertoire occupied by top-ranked clonotypes. Reveals the degree of clonal dominance.",
    detail = paste(
      "Imagine lining up all clonotypes from most common to least common, then asking: 'What percentage of all cells are accounted for by the top 10 clones? Top 100? Top 1000?'",
      "",
      "This plot answers exactly that, splitting clones into cumulative bins.",
      "",
      "What to look for:",
      "\u2022 If the top 10 clones already account for 50% of all cells, the repertoire is highly dominated by a few winners.",
      "\u2022 If even the top 1000 clones account for only a small fraction, the repertoire is very diverse and evenly distributed.",
      "\u2022 Comparing bars across samples shows which sample has more concentrated (or more spread out) immune responses.",
      "\u2022 This is a more intuitive way to see 'clonal dominance' than diversity indices.",
      sep = "\n"
    )
  ),
  Quant = list(
    short = "Unique clonotype count",
    summary = "Total number of distinct clonotypes per sample. Sensitive to sequencing depth; use Rarefaction for size-corrected comparison.",
    detail = paste(
      "The simplest possible question: just count how many distinct receptor sequences exist in each sample.",
      "",
      "What to look for:",
      "\u2022 More unique clonotypes usually = higher diversity.",
      "\u2022 Caution: this number is strongly influenced by how many cells were sequenced. A sample with 10,000 cells will naturally show more unique clonotypes than one with 1,000 cells, even if they're equally diverse. Use Rarefaction to correct for this.",
      "\u2022 Still useful for a quick first look \u2014 large differences between samples of similar size are meaningful.",
      "\u2022 In disease vs. healthy comparisons, reduced clonotype counts in disease tissue may indicate oligoclonal expansion.",
      sep = "\n"
    )
  ),
  Rarefaction = list(
    short = "Rarefaction analysis",
    summary = "Estimates clonotype discovery at subsampled depths. A plateauing curve confirms sufficient sequencing saturation.",
    detail = paste(
      "A critical quality check. Rarefaction asks: 'If we had sequenced fewer cells, how many unique clonotypes would we have found?'",
      "",
      "The plot simulates subsampling your data at different depths and counts clonotypes at each level.",
      "",
      "What to look for:",
      "\u2022 If the curve flattens (plateaus), you've sequenced enough \u2014 adding more cells won't reveal many new clonotypes.",
      "\u2022 If the curve is still rising steeply at the rightmost point, you haven't captured the full diversity and would benefit from sequencing more cells.",
      "\u2022 This is the proper way to compare clonotype counts between samples of different sizes \u2014 compare the curves at the same subsampled depth.",
      "\u2022 The shaded bands show confidence intervals from bootstrap resampling. Wider bands = more uncertainty.",
      "\u2022 Adjust the 'Bootstrap iterations' slider to trade speed for smoother confidence bands.",
      sep = "\n"
    )
  ),
  `Gene usage` = list(
    short = "V(D)J gene usage",
    summary = "Heatmap of V gene segment frequencies across samples. Biased usage may reflect antigen-driven selection or germline accessibility.",
    detail = paste(
      "T and B cell receptors are assembled by randomly joining gene segments called V (Variable), D (Diversity), and J (Joining).",
      "There are ~50+ V segments and ~10+ J segments in the genome, but not all are used equally.",
      "",
      "This heatmap shows how frequently each V gene segment is used across your samples. Rows = gene segments, columns = samples. Darker color = more usage.",
      "",
      "What to look for:",
      "\u2022 Some V genes are naturally used more often than others due to accessibility in the genome.",
      "\u2022 A V gene that is abnormally high in one condition may indicate antigen-driven selection \u2014 the immune system is preferentially expanding clones using that particular gene because it's good at recognizing a specific pathogen.",
      "\u2022 Differences in gene usage between healthy and disease samples can be disease biomarkers.",
      "\u2022 In B cells, comparing IGHV gene usage can reveal biases associated with specific antibody responses.",
      sep = "\n"
    )
  ),
  vizGenes = list(
    short = "Gene usage counts",
    summary = "Raw counts of V/J gene segment usage. Complements the percentage-based heatmap by showing absolute cell numbers.",
    detail = paste(
      "Similar to 'Gene usage' but displays raw counts instead of percentages in a heatmap.",
      "This gives you a sense of both how popular a gene segment is AND how many cells are involved.",
      "",
      "What to look for:",
      "\u2022 High count + high percentage = major gene usage that matters both relatively and absolutely.",
      "\u2022 High percentage but low count could be an artifact of a small sample.",
      "\u2022 Compare with 'Gene usage' (which shows percentages) to get both perspectives.",
      "\u2022 Useful when sample sizes differ significantly \u2014 raw counts show the actual data volume behind each percentage.",
      sep = "\n"
    )
  ),
  percentGenes = list(
    short = "Gene usage percentages",
    summary = "Normalised gene segment usage as a heatmap. Enables fair comparison across samples with different cell counts.",
    detail = paste(
      "Shows each V (or J) gene segment as a percentage of total usage, displayed as a heatmap where columns are samples/groups and rows are individual gene segments.",
      "",
      "What to look for:",
      "\u2022 Hot spots (bright cells) show gene segments that dominate in a particular sample.",
      "\u2022 Gene segments that are bright in disease but dim in healthy (or vice versa) suggest condition-specific gene usage bias.",
      "\u2022 This view is normalized, so it's fair to compare across samples even if they have different total cell counts.",
      "\u2022 Note: this uses the chain selected in the 'Chain' dropdown \u2014 if you see unexpected results, check that you've selected the right chain (e.g., TRB for TCR beta).",
      sep = "\n"
    )
  ),
  percentVJ = list(
    short = "V-J gene pairing",
    summary = "V-J combination frequency heatmap. Enriched pairings may indicate convergent selection for specific antigen-binding configurations.",
    detail = paste(
      "Each clonotype uses one V gene AND one J gene. This heatmap shows how often each V-J combination appears.",
      "Each panel (facet) represents a different sample or group.",
      "",
      "Why this matters:",
      "\u2022 V-J pairing is not random \u2014 some combinations are structurally favored.",
      "\u2022 If a specific V-J combination is unusually enriched in a disease sample, it suggests convergent selection: many independent cells arrived at the same receptor solution in response to the same antigen.",
      "\u2022 'Public' clonotypes (shared across individuals) often use the same V-J pairings.",
      "",
      "What to look for:",
      "\u2022 Bright spots in one panel but not others = condition-specific V-J preferences.",
      "\u2022 A diagonal pattern suggests V and J usage are correlated.",
      "\u2022 Broadly distributed color means diverse V-J usage with no strong preference.",
      sep = "\n"
    )
  ),
  `AA %` = list(
    short = "Positional amino acid composition",
    summary = "Amino acid frequency at each CDR3 position. Conserved positions suggest structural constraints; variable positions drive antigen specificity.",
    detail = paste(
      "The CDR3 region makes direct contact with antigens. This plot shows, at each position along the CDR3, what percentage of clonotypes use each amino acid.",
      "Each group gets its own facet panel, stacked vertically.",
      "",
      "What to look for:",
      "\u2022 Positions near the edges (start and end of CDR3) tend to be conserved \u2014 they are constrained by V and J gene segments.",
      "\u2022 Middle positions are usually more variable \u2014 this is where random nucleotide insertions create diversity.",
      "\u2022 If a certain amino acid dominates a middle position in your disease sample (but not in healthy), it may indicate selection for receptors that bind a specific antigen shape.",
      "\u2022 Glycine (G), Serine (S), and other small amino acids are common in CDR3 due to their structural flexibility.",
      "\u2022 Comparing between conditions reveals position-specific amino acid biases that could be functionally important.",
      sep = "\n"
    )
  ),
  Entropy = list(
    short = "Positional entropy",
    summary = "Shannon entropy at each CDR3 position. Low entropy indicates conservation; high entropy reflects sequence diversification.",
    detail = paste(
      "Entropy measures 'randomness' or 'uncertainty'. At each CDR3 amino acid position, if all clonotypes use the same amino acid, entropy is 0 (completely conserved). If every amino acid appears equally, entropy is maximal (completely random).",
      "",
      "This plot shows normalized Shannon entropy at each position.",
      "",
      "What to look for:",
      "\u2022 Low entropy positions (dips in the curve) are conserved \u2014 structural or functional constraints force most clonotypes to use the same amino acid there.",
      "\u2022 High entropy positions (peaks) are highly variable \u2014 these are the 'creative' spots where diversity is generated.",
      "\u2022 The edges of CDR3 (positions 1\u20133 and last 1\u20133) typically have lower entropy because they are encoded by the V and J gene segments.",
      "\u2022 Comparing entropy profiles between conditions: if a position becomes less random (lower entropy) in disease, it suggests selection pressure at that position.",
      "\u2022 This is a compact summary of the amino acid composition data shown in the 'AA %' tab.",
      sep = "\n"
    )
  ),
  Property = list(
    short = "CDR3 physicochemical profile",
    summary = "Mean physicochemical property values along the CDR3 region. Shifts between conditions reveal structural selection pressures.",
    detail = paste(
      "Amino acids differ in size, charge, hydrophobicity, and other physical/chemical properties. These properties determine how the CDR3 region interacts with antigens.",
      "",
      "This plot shows the average value of a physicochemical property at each CDR3 position, with confidence intervals. Choose different property scales from the dropdown:",
      "\u2022 Atchley Factors: 5 factors capturing size, polarity, charge, etc.",
      "\u2022 Kidera Factors: 10 factors from statistical analysis of amino acid properties.",
      "\u2022 Other scales (FASGAI, zScales, etc.) capture different aspects of amino acid chemistry.",
      "",
      "What to look for:",
      "\u2022 Regions with strong positive or negative property values indicate positions where specific physical properties are required (e.g., a hydrophobic core or a charged tip for antigen binding).",
      "\u2022 Wide confidence intervals mean high variability at that position.",
      "\u2022 Comparing between conditions: shifts in a property at specific positions suggest the disease selects for CDR3 with different physical characteristics.",
      "\u2022 This is particularly useful for understanding WHY certain receptors bind their targets \u2014 it goes beyond sequence to structure.",
      sep = "\n"
    )
  ),
  `K-mer` = list(
    short = "CDR3 k-mer motifs",
    summary = "Top recurring short amino acid subsequences in CDR3. Condition-enriched motifs may mark antigen-specific binding signatures.",
    detail = paste(
      "A 'k-mer' is a short subsequence of fixed length (default: 3 amino acids). This analysis scans all CDR3 sequences, counts every possible 3-amino-acid window, and shows the most frequent ones.",
      "",
      "Think of it as finding the most popular 'building blocks' or 'words' within CDR3 sequences.",
      "",
      "What to look for:",
      "\u2022 Motifs that are frequent in one condition but rare in another may be functionally relevant \u2014 they could be part of the antigen-binding site.",
      "\u2022 Shared motifs across samples suggest 'public' immune responses where different individuals use similar receptor sequences against the same threat.",
      "\u2022 Some motifs are common simply because they arise naturally from popular V/J gene segments.",
      "\u2022 Use the slider to show more or fewer top motifs. Start with 15\u201330 for an overview, increase to 50+ for deeper analysis.",
      sep = "\n"
    )
  ),
  Compare = list(
    short = "Clonotype tracking",
    summary = "Alluvial diagram tracking top clonotypes across samples. Shared ribbons represent public or persistent clones.",
    detail = paste(
      "This alluvial (flow) diagram tracks the top clonotypes across your selected samples.",
      "Each colored ribbon represents a clonotype, and its height represents its proportion.",
      "",
      "What to look for:",
      "\u2022 Ribbons that flow across multiple samples = 'public' or shared clonotypes, present in both samples.",
      "\u2022 Ribbons that appear in only one sample = 'private' clonotypes, unique to that sample.",
      "\u2022 In longitudinal studies (same patient, different time points), shared clonotypes represent persistent immune memory.",
      "\u2022 In different patients, shared clonotypes suggest convergent immune responses to the same antigen.",
      "\u2022 The height of each ribbon shows how dominant that clone is \u2014 a thick ribbon across both samples means a highly expanded public clone.",
      sep = "\n"
    )
  ),
  Overlap = list(
    short = "Repertoire overlap",
    summary = "Pairwise clonotype sharing between samples. High overlap indicates similar immune responses or active cell trafficking.",
    detail = paste(
      "This heatmap shows pairwise overlap between every combination of samples.",
      "Overlap is calculated as the fraction of clonotypes shared between two samples.",
      "",
      "What to look for:",
      "\u2022 Dark/high values mean two samples share many clonotypes \u2014 their immune responses are similar.",
      "\u2022 Light/low values mean the samples have mostly distinct clonotypes.",
      "\u2022 The diagonal is always maximal (a sample perfectly overlaps with itself).",
      "\u2022 In treatment studies: high overlap between pre- and post-treatment suggests the treatment didn't dramatically reshape the repertoire.",
      "\u2022 In tissue comparisons: high overlap between blood and tumor suggests active immune cell trafficking.",
      "\u2022 Symmetric matrix: overlap(A,B) = overlap(B,A).",
      sep = "\n"
    )
  ),
  Scatter = list(
    short = "Clone frequency scatter",
    summary = "Scatterplot comparing clonotype frequencies between two samples. Off-diagonal clones have expanded or contracted.",
    detail = paste(
      "Each dot is a clonotype. The X-axis shows its frequency in one sample; the Y-axis shows its frequency in another.",
      "",
      "What to look for:",
      "\u2022 Dots on the diagonal: clones equally abundant in both samples (stable clones).",
      "\u2022 Dots above the diagonal: clones expanded in the Y-axis sample relative to the X-axis sample.",
      "\u2022 Dots below the diagonal: clones expanded in the X-axis sample.",
      "\u2022 Dots along the X-axis only (Y near 0): clones found only in the first sample.",
      "\u2022 Dots along the Y-axis only (X near 0): clones found only in the second sample.",
      "\u2022 Larger dots indicate clones with higher total abundance across both samples.",
      "\u2022 This is one of the most intuitive plots for identifying clones that expand or contract between conditions.",
      sep = "\n"
    )
  ),
  SizeDist = list(
    short = "Clone size distribution clustering",
    summary = "Hierarchical clustering of samples by clone size distribution. Samples that cluster together share similar repertoire architecture.",
    detail = paste(
      "This dendrogram clusters samples based on how similar their clone size distributions are.",
      "It uses Ward's hierarchical clustering method \u2014 samples that branch together early have the most similar patterns.",
      "",
      "What to look for:",
      "\u2022 Samples that cluster together share similar 'shapes' of repertoire \u2014 e.g., both dominated by a few big clones, or both having many small clones.",
      "\u2022 If disease samples cluster separately from healthy samples, it suggests disease systematically changes the repertoire structure.",
      "\u2022 If replicates or time points from the same patient cluster together, it confirms biological consistency.",
      "\u2022 Long branch lengths between clusters mean large differences in repertoire structure.",
      "\u2022 This provides a global summary of repertoire 'shape' without focusing on specific clonotypes.",
      sep = "\n"
    )
  ),
  Isotype = list(
    short = "BCR isotype distribution",
    summary = "Stacked-bar of IgM/IgD/IgG1-4/IgA1-2/IgE proportions per sample or timepoint. Class-switch readout unique to BCR.",
    detail = paste(
      "After antigen stimulation in germinal centres, B cells can switch their antibody isotype (class-switch recombination) from IgM/IgD to IgG, IgA, or IgE.",
      "This plot shows the proportion of each isotype in every sample or timepoint.",
      "",
      "What to look for:",
      "\u2022 A high IgM/IgD fraction indicates na\u00efve or unswitched B cells.",
      "\u2022 Increased IgG (especially IgG1/IgG3) suggests T-cell-dependent immune activation.",
      "\u2022 IgA enrichment is typical in mucosal tissues or chronic inflammation.",
      "\u2022 Shifts from IgM-dominant to IgG/IgA-dominant between timepoints indicate ongoing germinal centre maturation.",
      "\u2022 This analysis is BCR-specific \u2014 TCR data does not have isotype information.",
      sep = "\n"
    )
  ),
  `SHM Proxy` = list(
    short = "Somatic hypermutation proxy",
    summary = "Within-clone CDR3-H3 nucleotide diversity as a proxy for SHM activity. Higher diversity per clone family implies more mutations.",
    detail = paste(
      "Somatic hypermutation (SHM) introduces point mutations in BCR variable regions during germinal centre reactions, enabling affinity maturation.",
      "This plot approximates SHM activity by counting unique IGH CDR3 nucleotide sequences within each clone family (size >= 2 cells).",
      "",
      "What to look for:",
      "\u2022 Higher within-clone CDR3 nt diversity implies more SHM events in that clone family.",
      "\u2022 Comparing timepoints: an increase in diversity suggests active affinity maturation.",
      "\u2022 Clones with diversity = 1 have identical CDR3-H3 nt across all member cells (no observed SHM in CDR3).",
      "\u2022 This is a proxy metric \u2014 precise SHM quantification requires IgBLAST / Change-O alignment to germline.",
      "\u2022 Only clone families with >= 2 cells are included to avoid singletons that carry no intra-clonal information.",
      sep = "\n"
    )
  ),
  `Paired Scatter` = list(
    short = "Per-subject Pre vs Post clone scatter",
    summary = "Faceted scatter comparing clonotype frequencies between Pre and Post timepoints for each subject. Off-diagonal clones expanded or contracted.",
    detail = paste(
      "Each panel shows one subject. Every dot is a clonotype; X = frequency in Pre, Y = frequency in Post.",
      "This directly answers whether treatment changed the clonal structure.",
      "",
      "What to look for:",
      "\u2022 Dots on the diagonal \u2014 stable clones, unchanged by treatment.",
      "\u2022 Dots above the diagonal \u2014 clones that expanded after treatment.",
      "\u2022 Dots below the diagonal \u2014 clones that contracted after treatment.",
      "\u2022 Dots along only one axis \u2014 clones unique to Pre or Post (appeared or disappeared).",
      "\u2022 Compare patterns across subjects: consistent shifts suggest a shared treatment effect.",
      "\u2022 Requires paired Pre/Post samples from the same subject_id in the data.",
      sep = "\n"
    )
  )
)

## ---- Collapsible help panel ------------------------------------------- ##
output$ir_help_panel <- renderUI({
  tab <- input$ir_tabs
  if (is.null(tab)) {
    return(NULL)
  }
  info <- ir_tab_help[[tab]]
  if (is.null(info)) {
    return(NULL)
  }
  div(
    style = "background: #e5f0fa; border-left: 4px solid #0f6cbd; padding: 8px 12px; margin-bottom: 10px; font-size: 13px; border-radius: 2px; display: flex; align-items: flex-start; gap: 10px;",
    div(
      style = "flex: 1;",
      tags$strong(info$short),
      tags$p(
        style = "margin: 4px 0 0 0; color: var(--neutral-secondary);",
        info$summary
      )
    ),
    actionButton(
      "ir_help_example_btn",
      label = tags$span(icon("lightbulb"), " Example"),
      class = "btn-xs",
      style = "white-space: nowrap; margin-top: 2px; background: #0f6cbd; color: #fff; border: none;"
    )
  )
})

## ---- Demo data (lazy, cached) ----------------------------------------- ##
ir_demo_data <- reactiveVal(NULL)

.get_demo_data <- function() {
  if (!is.null(ir_demo_data())) {
    return(ir_demo_data())
  }
  tryCatch(
    {
      data("contig_list", package = "scRepertoire", envir = environment())
      demo <- scRepertoire::combineTCR(
        contig_list[1:2],
        samples = c("Healthy", "Disease")
      )
      ir_demo_data(demo)
      demo
    },
    error = function(e) NULL
  )
}

## ---- BCR demo data (synthetic — scRepertoire has no built-in BCR) ---- ##
## The Isotype and SHM Proxy tabs require BCR (IGH) data. We generate a
## realistic synthetic dataset with two samples: Pre-vaccination (mostly
## IgM/IgD, naive B cells) and Post-vaccination (class-switched to IgG/IgA).
ir_bcr_demo_data <- reactiveVal(NULL)

.get_bcr_demo_data <- function() {
  if (!is.null(ir_bcr_demo_data())) {
    return(ir_bcr_demo_data())
  }
  set.seed(42)
  n <- 200L

  make_ctgene <- function(iso) {
    v <- sample(c("IGHV1-2", "IGHV1-18", "IGHV3-23", "IGHV3-30", "IGHV4-34"), 1L)
    d <- sample(c("IGHD2-2", "IGHD3-10", "IGHD6-13"), 1L)
    j <- sample(c("IGHJ4", "IGHJ5", "IGHJ6"), 1L)
    paste(v, d, j, iso, sep = "_")
  }

  make_nt <- function() {
    paste(sample(c("A", "T", "G", "C"), 300L, replace = TRUE), collapse = "")
  }

  make_bc <- function(i, prefix) {
    paste0(prefix, "_BCR_", sprintf("%04d", i))
  }

  make_df <- function(prefix, iso_probs) {
    isos <- sample(names(iso_probs), n, replace = TRUE, prob = iso_probs)
    clones <- sample(1L:40L, n, replace = TRUE)
    data.frame(
      barcode    = vapply(seq_len(n), function(i) make_bc(i, prefix), ""),
      CTgene     = vapply(isos, make_ctgene, ""),
      CTnt       = vapply(seq_len(n), function(i) make_nt(), ""),
      CTaa       = paste0(
        "C", vapply(seq_len(n), function(i) {
          paste(sample(
            strsplit("ARNDCQEGHILKMFPSTWYV", "")[[1]],
            15L, replace = TRUE
          ), collapse = "")
        }, "")
      ),
      CTstrict   = vapply(clones, function(c) sprintf("IGH_clone_%03d", c), ""),
      sample     = prefix,
      cloneSize  = sample(1L:8L, n, replace = TRUE),
      stringsAsFactors = FALSE
    )
  }

  demo <- list(
    "Pre-vaccination" = make_df("Pre-vaccination", c(
      IGHM = 0.60, IGHD = 0.20, IGHG1 = 0.10, IGHG2 = 0.05, IGHA1 = 0.05
    )),
    "Post-vaccination" = make_df("Post-vaccination", c(
      IGHM = 0.20, IGHD = 0.05, IGHG1 = 0.30, IGHG2 = 0.15,
      IGHG3 = 0.10, IGHA1 = 0.15, IGHE = 0.05
    ))
  )
  ir_bcr_demo_data(demo)
  demo
}

## ---- Example modal ---------------------------------------------------- ##
observeEvent(input$ir_help_example_btn, {
  tab <- input$ir_tabs
  if (is.null(tab)) {
    return()
  }
  info <- ir_tab_help[[tab]]
  if (is.null(info)) {
    return()
  }

  showModal(modalDialog(
    title = paste0("Example: ", tab),
    size = "l",
    easyClose = TRUE,
    fade = TRUE,
    div(
      div(
        style = "font-size: 14px; margin-bottom: 12px;",
        .format_detail(info$detail)
      ),
      tags$hr(),
      tags$p(
        style = "color: var(--neutral-tertiary); font-size: 12px;",
        "Generated from scRepertoire built-in demo data (2 TCR samples: Healthy vs Disease)."
      ),
      plotOutput("ir_demo_plot", height = "450px")
    ),
    footer = modalButton("Close")
  ))
})

## ---- Demo plot renderer ----------------------------------------------- ##
output$ir_demo_plot <- renderPlot({
  req_plot_space("ir_demo_plot")
  tab <- input$ir_tabs
  if (is.null(tab)) {
    plot.new()
    text(0.5, 0.5, "Demo data unavailable", cex = 1.2)
    return()
  }
  # BCR-specific tabs use synthetic BCR data; all others use scRepertoire's
  # built-in TCR demo.
  is_bcr_tab <- tab %in% c("Isotype", "SHM Proxy")
  demo <- if (is_bcr_tab) .get_bcr_demo_data() else .get_demo_data()
  if (is.null(demo)) {
    plot.new()
    text(0.5, 0.5, "Demo data unavailable", cex = 1.2)
    return()
  }
  tryCatch(
    {
      p <- switch(
        tab,
        "Abundance" = scRepertoire::clonalAbundance(demo, cloneCall = "gene"),
        "Diversity" = ir_plot_clonal_diversity(
          data = demo,
          clone_call = "gene",
          chain = "TRB",
          group_by = NULL,
          metric = "shannon",
          x_axis = NULL,
          n_boots = 20,
          palette = "inferno"
        ),
        "Homeostasis" = scRepertoire::clonalHomeostasis(
          demo,
          cloneCall = "gene",
          chain = "TRB",
          palette = "inferno"
        ),
        "Length" = scRepertoire::clonalLength(
          demo,
          cloneCall = "aa",
          chain = "TRB",
          palette = "inferno"
        ),
        "Proportion" = scRepertoire::clonalProportion(
          demo,
          cloneCall = "gene",
          chain = "TRB",
          palette = "inferno"
        ),
        "Quant" = scRepertoire::clonalQuant(
          demo,
          cloneCall = "gene",
          chain = "TRB",
          scale = FALSE,
          palette = "inferno"
        ),
        "Rarefaction" = scRepertoire::clonalRarefaction(
          demo,
          cloneCall = "gene",
          chain = "TRB",
          n.boots = 3,
          palette = "inferno"
        ),
        "Gene usage" = scRepertoire::percentGeneUsage(
          demo,
          chain = "TRB",
          gene = "TRBV",
          plot.type = "heatmap",
          palette = "inferno"
        ),
        "vizGenes" = scRepertoire::vizGenes(
          demo,
          x.axis = "TRBV",
          y.axis = NULL,
          plot = "heatmap",
          palette = "inferno"
        ),
        "percentGenes" = scRepertoire::percentGenes(
          demo,
          chain = "TRB",
          gene = "Vgene",
          palette = "inferno"
        ),
        "percentVJ" = scRepertoire::percentVJ(
          demo,
          chain = "TRB",
          palette = "inferno"
        ),
        "AA %" = scRepertoire::percentAA(
          demo,
          chain = "TRB",
          aa.length = 20,
          palette = "inferno"
        ),
        "Entropy" = scRepertoire::positionalEntropy(
          demo,
          chain = "TRB",
          aa.length = 20,
          palette = "inferno"
        ),
        "Property" = scRepertoire::positionalProperty(
          demo,
          chain = "TRB",
          method = "atchleyFactors",
          palette = "inferno"
        ),
        "K-mer" = scRepertoire::percentKmer(
          demo,
          chain = "TRB",
          cloneCall = "aa",
          motif.length = 3,
          top.motifs = 15,
          palette = "inferno"
        ),
        "Compare" = scRepertoire::clonalCompare(
          demo,
          cloneCall = "gene",
          chain = "TRB",
          samples = names(demo),
          top.clones = 5,
          graph = "alluvial",
          palette = "inferno"
        ),
        "Overlap" = scRepertoire::clonalOverlap(
          demo,
          cloneCall = "gene",
          chain = "TRB",
          method = "overlap",
          palette = "inferno"
        ),
        "Scatter" = scRepertoire::clonalScatter(
          demo,
          cloneCall = "gene",
          chain = "TRB",
          x.axis = names(demo)[1],
          y.axis = names(demo)[2],
          palette = "inferno"
        ),
        "SizeDist" = scRepertoire::clonalSizeDistribution(
          demo,
          cloneCall = "gene",
          method = "ward.D2"
        ),
        "Isotype" = bcr_isotype_plot(demo, group_col = "sample"),
        "SHM Proxy" = bcr_shm_proxy_plot(demo, group_col = "sample"),
        {
          plot.new()
          text(0.5, 0.5, paste("No example available for:", tab), cex = 1.2)
        }
      )
      if (inherits(p, "gg")) print(p)
    },
    error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error generating example:\n", e$message), cex = 0.9)
    }
  )
})
