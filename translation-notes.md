# Translation note
## Greco-Roman words
Latin words are rendered with Classical pronunciation in Japanese.  E.g., Cicero キケロー, Tacitus タキトゥス.

Greek words are rendered with Classical pronunciation in Japanese, where aspiration is denoted with `\jdb{*}`.  E.g., Ἀθῆναι ア\jdb{テ}ーナイ, Θουκυδίδης \jdb{ト}ゥーキューディデース.

### Proper-name consistency
Prefer the same Classical-pronunciation policy in prose, headings, and examples.

| de | ja-tex | note |
| --- | --- | --- |
| Cicero | キケロー | Avoid シケロ. |
| Tacitus | タキトゥス |  |
| Sophokles | ソ\jdb{ポ}クレース |  |
| Perikles | ペリクレース |  |
| Platon / Plato | プラトーン | Avoid プラトン if consistency with long vowels is desired. |
| Aristophanes | アリスト\jdb{パ}ネース | Avoid アリストパネス. |
| Thukydides | \jdb{ト}ゥーキューディデース |  |
| Erasmus | エラスムス | Latinized Renaissance name; エラスモス is also possible, but do not mix. |
| Wilhelm Roscher | ヴィルヘルム・ロッシャー |  |
| Emil de Laveleye | エミール・ド・ラヴェレー | Confirm French reading; current ラヴェレイ is possible but inconsistent with usual French final `-eye` guesswork. |
## German-specific phrases
Some phrases do not make sense when translating in Japanese.  For example,

> Der Deutsche fragt: \emph{Wohin} setzt er sich? der Grieche: \emph{Wo?}
> Wohin wollen wir uns setzen? \textgreek[variant=ancient]{ποῦ καθιζησόμεθα;}

Could be translated as

> ドイツ人は「彼は\emph{どこへ}座るのか」と問うが、ギリシア人は「\emph{どこに}」と問う。
> 私たちはどこに座ろうか？ \textgreek[variant=ancient]{ποῦ καθιζησόμεθα;}

whereas unfortunately Japanese say どこに too.  We can use a ruby like to clarify the difference from German like this.

> ドイツ人は「彼は\emph{\ruby{どこへ}{Wohin}}座るのか」と問うが、ギリシア人は「\emph{どこに}」と問う。
> 私たちはどこに座ろうか？ \textgreek[variant=ancient]{ποῦ καθιζησόμεθα;}

## Lexicon
de: Griechisch (Sprache)
ja: ギリシャ語

de: Athener
ja-tex: ア\jdb{テ}ーナイ人

de: Attiker (person)
ja-tex: ア\jdb{テ}ーナイ人
note: Use the Japanese pattern “singular nominative place name + 人”, not アッティカ人.

de: Aristophanes
ja-tex: アリスト\jdb{パ}ネース

## `legend-ja.tex` proofreading notes

Checked source: `legend-ja.tex`, especially after `\todo{この節ここまで校了}` at line 91.

### Possible source German / OCR issues

| line | source | issue | suggestion |
| --- | --- | --- | --- |
| 43-44 | `sophokleïscher Dramen und perikleïsche Reden` | Case agreement looks suspicious after `zum Verständniß`; likely should be parallel genitive. | Treat as “ソポクレースの劇とペリクレースの演説の理解”. If editing German source, consider `perikleïscher Reden`. |
| 122-124 | `die Stimme des gemeinsten Mannes, --- schon dies nöthigt sie` | `sie` probably refers back to the writers/dialogues just mentioned, but the antecedent is loose. | Translate by making the implicit subject explicit: “劇作家やプラトーンは、ごく普通の人の声を扱う以上、その言葉遣いに近くとどまらざるを得ない”. |

### Japanese translation issues after the checked marker

| line | current | issue | suggested direction |
| --- | --- | --- | --- |
| 106 | `アリストパネス` | Inconsistent with the transcription policy. | `アリスト\jdb{パ}ネース` |
| 106 | `語法から取った` | `Sprache` here is broader than “usage”. | `アリストパネースの言葉から採った` or `アリストパネースの言語から採った` |
| 107 | `後代のギリシャ語` | Good, but this should be the stable rendering of `spätere Gräcität`. | Add to lexicon as `後代ギリシャ語` / `後代のギリシャ語`. |
| 108 | `補遺` | `Ergänzungen` means additions/supplements to the phrasebook, not necessarily an appendix. | `補足語句` or `補足された語句` |
| 131 | `ギリシャ語解` | Typo/unnatural. | `ギリシャ語理解` |
| 131 | `アッティカ語で会話を学ぶ` | `attisch conversiren zu lernen` means learning to converse in Attic, not learning conversation itself. | `アッティカ語で会話できるようになる` |
| 133 | `シケロ` | Inconsistent with notes. | `キケロー` |
| 133 | `その中に民衆語が...` | Sentence connection is awkward; `in welchen` refers to the Latin works. | `それらの中では民衆語はところどころにかすかに認められるだけである` |
| 135 | `プラトン` | Inconsistent with long-vowel Greek proper-name policy. | `プラトーン` |
| 136-137 | `最も庶民的な人々の声が...強い` | Subject of `nöthigt sie` is not the voice itself; likely the authors/dialogues are compelled to stay close to ordinary speech. | `ごく普通の人の声を扱うこと自体が、彼らをその言葉遣いの近くにとどまらせる` |
| 180 | `ギリシャ方言口語` | Source says `griechischen Umgangssprache`, not specifically Attic here. | `ギリシャ語の口語` |
| 181 | `語の置き換え` | `Wortvergleichungen` is comparison/correspondence of words, not replacement. | `語句の比較` / `語句の対応づけ` |
| 184 | `平凡な表現` | `alltägliche Ausdrucksweise` is “everyday expression”; `平凡` may sound evaluative. | `日常的な表現` |
| 185 | `この勉強` | A little colloquial for the register. | `この学習` or `この研究` |
| 241-244 | `たとえば「veniam」と「ibo」を区別できないとすれば` | This Latin comparison has been pulled into the preceding Greek example; German first asks whether learners can produce the Attic expression for “I will come to you”, then separately compares Latin `veniam`/`ibo`. | End the Greek sentence before the Latin example: “...どう表すかを自信を持って言える人は驚くほど少ない。” Then start the Latin comparison. |
| 253 | `ギリシャ語の修了証` | `Zeugniß der Reife im Griechischen` is a school-leaving/maturity certificate showing Greek attainment, not a “Greek completion certificate”. | `ギリシャ語で成熟証書を得て学校を出た若者` or less literal `ギリシャ語履修を含む修了証を得て学校を出た若者` |
| 255 | `話術` | `Sprachfertigkeit` is language/speech proficiency, not rhetoric. | `会話能力` / `言語運用能力` |
| 256-257 | `全く身から離れている` | Unnatural Japanese for `so völlig fern zu bleiben scheint`. | `まったく縁遠いままに見える` |
| 258 | `簡単な思考` | `Gedanken` is better as thoughts/ideas. | `ごく簡単な考え` |
| 261 | `自在に扱って意思疎通できる` | Slightly stronger than `so zu bemächtigen, daß man sich darin verständlich machen könnte`. | `意思疎通できる程度にギリシャ語を身につけられる` |
| 277 | `日常生活の共通語` | `Verkehrssprache` is not “common language” in the sociolinguistic sense; it means language of daily intercourse. | `日常生活の交際語` / `日常のやりとりの言葉` |
| 278 | `語の備蓄` | Unnatural for `Urvorrath`. | `根本語彙` / `基礎語彙の蓄え` |
| 278 | `作られ、接続してきた` | `angesetzt und angeschlossen` evokes formations attaching/growing around a nucleus. | `付着し、結びついてきた` |
| 279 | `言語をつかもうとする者が本当に学ぶべきものがある` | Syntax reverses the force of `Hier gilt es, die Sprache zu fassen`. | `本当に学ぼうとする者は、ここでこそ言語を捉えなければならない` |
| 301 | `ライプツィヒ大学の` | Source says `berühmte Leipziger Nationalökonom`; “university” is an inference. | If staying close to source: `有名なライプツィヒの経済学者` |
| 319 | `ローマの元老院語` | Understandable but stiff/opaque. | `ローマ元老院の言葉` |
| 320 | `もっとも貴い金属` | `Erz` is ore/metal; the metaphor is smelting/refining. | `最も高貴な鉱金へと溶け合ってい
る` or freer `最も高貴な金属へと鍛え上げられている` |

### Terms to keep consistent

| German | preferred Japanese | avoid / note |
| --- | --- | --- |
| attische Umgangssprache | アッティカ方言口語 | Keep this for the book’s central term. |
| griechische Umgangssprache | ギリシャ語の口語 | Do not automatically render as `ギリシャ方言口語`. |
| Verkehrssprache des täglichen Lebens | 日常生活の交際語 / 日常のやりとりの言葉 | Avoid `共通語` unless the German is `Gemeinsprache` or similar. |
| gemeiner Mann | 民衆 / 庶民 / ごく普通の人 | In the preface, `gemein` is “ordinary/common”, not morally low. |
| gemeinst | ごく普通の / 最も庶民的な | Choose based on register; avoid suggesting vulgarity unless context demands it. |
| spätere Gräcität | 後代のギリシャ語 | Stable rendering for non-classical later Greek. |
| Neugriechisch | 現代ギリシャ語 | Existing line 108 is fine. |
| Ergänzungen | 補足語句 / 補足された語句 | Avoid `補遺` if it sounds like a separate appendix. |
| Sprachfertigkeit | 会話能力 / 言語運用能力 | Avoid `話術`. |
| Wortvergleichung(en) | 語句の比較 / 語句の対応づけ | Avoid `語の置き換え`. |
| Geist der Sprache | 言語の精神 | Existing rendering is acceptable. |
| jocoser Ton | 滑稽な調子 | Existing rendering is acceptable. |
| Urvorrath | 根本語彙 / 基礎語彙の蓄え | Avoid `語の備蓄`. |
| Krystallisationskern | 結晶化の核 | Existing rendering is acceptable. |

## `grammar-ja.tex` notes

Checked source: `grammar-ja.tex`, especially after `\todo{この節ここまで校了}` at line 43.

### Passive-voice section

The early grammar section argues that Greek often avoids passive forms which are identical with middle forms. Keep the Japanese focused on “how Greek replaces a German passive idea” rather than forcing every example into a literal passive.

| German | preferred Japanese | note |
| --- | --- | --- |
| Passivum | 受動 / 受動形 / 受動態 | Use `受動形` when the form is at issue, `受動` for the construction or meaning. |
| mediale Formen | 中動態形 / 中動態と同形 | In the warning sentence, `中動態と同形の受動形` is compact and clear. |
| Umschreibungen des Passivums | 受動の迂言 | `言い換え` is fine in prose, but `迂言` matches grammatical register. |
| durch active Verba | 能動動詞による | Examples should remain German passive idea + Greek idiom. |
| durch Substantiva mit Verben | 名詞と動詞による | Avoid overexplaining as “名詞と動詞の組み合わせで言い換える” in headings. |
| durch Adjektiva mit εἶναι | 形容詞＋εἶναιによる |  |
| übertragen werden | 移される / 譲渡される | In the generic `γίγνεσθαι` list, `移される` is broad and avoids overcommitting. |
| gefeiert werden (von Festen) | （祭りが）祝われる | `執り行われる` is possible, but `祝われる` is closer to `gefeiert`. |
