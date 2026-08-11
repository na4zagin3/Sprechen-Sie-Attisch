# Translation review plan for parts E–I

## Aim

Revise the Japanese as natural dialogue while remaining faithful to both the German prompt and the
Ancient Greek expression. The German and Greek are related evidence, but neither is automatically a
literal translation of the other. When they differ, the review must identify the difference before
choosing a Japanese formulation.

## Order of work

Review one section at a time, normally in this order:

1. Reconstruct the likely scene, turn groups, speakers, addressees, and level of politeness. Treat a
   run as continuous dialogue only when the wording supports continuity; otherwise regard its entries
   as alternative model phrases.
2. Analyse the German independently. Check historical meanings, idioms, ellipsis, register, and
   nineteenth-century spelling or usage. Do not silently modernize a construction merely because it
   is unusual today.
3. Analyse the Greek independently with LSJ and the corpora. Record morphology, syntax, particles,
   pragmatic force, and any difference between the handbook phrase and its attested source.
4. Locate attestations where practical. Prefer the canonical corpus for citation and use the larger
   discovery corpus to find candidates. Store the original Greek text, reference, local file, nearby
   context, speaker and scene when recoverable, and classify the relationship as exact, near-exact,
   adapted, syntactic parallel, formulaic parallel, or lexical only.
5. Compare the German and Greek explicitly. Record mismatches such as literal versus idiomatic
   wording, changes of person or number, altered speech act, added contextual information, and source
   or printing errors. Do not conceal a real mismatch by forcing one language into the other.
6. Draft Japanese for the communicative act of the whole utterance. Preserve distinctions between
   neighbouring entries, especially contrasts such as doing versus trying, shared discovery versus
   personal verification, or literal statement versus impatient command.
7. Read each proposed turn in its local dialogue run. Keep the estimated actor's register and tone
   consistent, but do not invent gender, intimacy, aggression, or fixed speakers where the evidence
   is weak.
8. Discuss uncertain or materially interpretive entries with the user. Mark accepted wording only
   after review, then synchronize accepted Japanese into the source `part-*.yaml` without overwriting
   unrelated edits.

## German research policy

- Search historical dictionaries and dated text corpora for obsolete or suspicious expressions.
- Preserve German attestations in the entry's `attestations`, not merely in discussion. Record the
  quoted construction, bibliographic reference or stable URL, date when known, sufficient surrounding
  text to establish its meaning, and the relationship between the attestation and handbook wording.
- Check the whole construction, not just isolated modern dictionary senses. Pay special attention to
  particles and discourse words such as *doch*, *nun*, *gleich*, *einmal*, *schon*, and *denn*.
- Distinguish an attested idiom from a plausible interpretation inferred mainly from the Greek.
- Record spelling or grammar that may be a source error under `textual_issues`; preserve the printed
  German in `source` unless a correction is explicitly accepted.
- In the rationale, state whether the Japanese chiefly follows the German, the Greek, or a natural
  formulation that mediates between them.

## Greek research policy

- Consult LSJ before settling disputed senses and cite the relevant sense division when useful.
- Prefer an attested dramatic or prose context over a decontextualized gloss.
- Preserve particles and verbal aspect in the analysis even when Japanese expresses their force
  through word order, punctuation, or sentence ending.
- Mark adaptations honestly: omitted words, changed person or tense, normalized dialect, or a phrase
  extracted from a longer sentence must not be labelled exact.

## Evidence recorded in review YAML

Research that affects a translation decision must remain reproducible from the review YAML rather
than surviving only in chat. Each relevant `attestations` item should record, as applicable:

- `language`: `grc` or `de`;
- the handbook `target` and `match_type`;
- author, work, passage, date or edition;
- `local_file` or stable URL;
- the original Greek or German text, plus enough neighbouring context to justify the interpretation;
- original speaker and scene when dialogue supplies pragmatic evidence;
- `relationship_to_handbook`, including every omission, substitution, inflectional change, or other
  adaptation;
- `confidence` and a concise `research_method`.

Dictionary evidence should identify the dictionary, headword, sense or subsection, and the relevant
example without overstating a definition as an attestation of the complete phrase. If no suitable
attestation is found, record `match_type: not-found`, the corpora or dictionaries searched, and the
resulting uncertainty. Interpretations inferred mainly by comparing German and Greek must be labelled
as inference in `review_comments` or the translation rationale.

## Japanese standard

- Natural spoken Japanese takes priority over word-for-word alignment, but no semantic component may
  be dropped without a recorded reason.
- Literary or classical colouring should come from restrained diction and rhythm, not indiscriminate
  archaism.
- Prefer stable Japanese terms or German-derived terminology where the domain calls for it; avoid
  unnecessary English borrowing.
- Use pronouns only when contrast, emphasis, or turn clarity requires them.
- Re-evaluate clusters of near-synonymous phrases together so each item has a distinct function in
  Japanese.
- Do not equate a speaker's social register with a single sentence ending. A speaker who uses polite
  forms toward an interlocutor may naturally use plain forms in a soliloquy, exclamation, quotation,
  or self-directed remark. Record such shifts as scene-dependent rather than treating them as voice
  inconsistency.
- Before smoothing a sequence, decide whether adjacent entries are genuine consecutive turns,
  alternative replies to the same prompt, or interchangeable versions of one speech act. Store this
  structure in `turn_groups` and do not manufacture a continuous exchange from alternatives.
- Where speaker allocation materially determines wording, record the candidate speaker and confidence.
  Prefer a provisional assignment supported by the surrounding turns to an unmarked assumption, and
  preserve uncertainty when two allocations remain plausible.
- Remember that the handbook entries are reusable model phrases as well as pieces of reconstructed
  dialogue. A translation should normally remain intelligible when read independently. If natural
  dialogue omits a subject, object, copula, or other recoverable element that a learner may need,
  supply it economically in Japanese parentheses rather than burdening the spoken phrase. Parentheses
  should mark genuinely optional supplementation, not hide uncertainty about the meaning.

## Completion criteria for a section

A section is ready for user review when its scene reconstruction is adequate, every entry has been
checked against both German and Greek, difficult usages have research notes or attestations, proposed
Japanese reads coherently by turn group, and unresolved items are explicitly marked rather than
silently guessed.
