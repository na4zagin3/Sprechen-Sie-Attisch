-- LuaTeX-ja processes the list after luaotfload and protects the resulting
-- glyph nodes.  For UniFraktur this restores the original Latin characters,
-- undoing long-s substitutions and the required ch/tz compositions before
-- luaotfload's letterspacing callback runs.
--
-- Keep the normal shaping pass for every font, then unprotect and reshape only
-- UniFraktur after LuaTeX-ja.  Letterspacing must run after that second pass.

if rawget(_G, 'attisch_unifraktur_reshaper_installed') then
  return
end
attisch_unifraktur_reshaper_installed = true

local function is_unifraktur(glyph)
  local data = font.getfont(node.direct.getfont(glyph))
  return data and data.fullname == 'UnifrakturMaguntia'
end

-- In TU encoding, \textcompwordmark is U+200C.  HarfBuzz suppresses its
-- glyph, but UniFraktur does not regard it as a word boundary: the s before it
-- therefore receives the long form.  A zero-width kern separates the shaping
-- runs instead.  It also lets letterspacing add exactly one interval across
-- the compound boundary.
local function replace_unifraktur_wordmarks(head)
  local direct = node.direct
  local direct_head = direct.todirect(head)
  local wordmarks = {}
  local kern_id = node.id('kern')

  for glyph in direct.traverse_glyph(direct_head) do
    local char = direct.getchar(glyph)
    local compensation = direct.getnext(glyph)
    local unifraktur = is_unifraktur(glyph)
    local shaped_wordmark =
      char == 32 and unifraktur
      and compensation and direct.getid(compensation) == kern_id
      and direct.getkern(compensation) == -direct.getwidth(glyph)

    if unifraktur and (char == 0x200C or shaped_wordmark) then
      wordmarks[#wordmarks + 1] = {glyph, shaped_wordmark and compensation}
    end
  end

  for _, wordmark in ipairs(wordmarks) do
    local glyph, compensation = wordmark[1], wordmark[2]
    local boundary = direct.new(kern_id, 1)
    direct.setkern(boundary, 0)
    direct_head = direct.insert_before(direct_head, glyph, boundary)
    if compensation then
      direct_head = direct.remove(direct_head, compensation, true)
    end
    direct_head = direct.remove(direct_head, glyph, true)
  end

  return direct.tonode(direct_head)
end

local function unprotect_unifraktur(head)
  local direct = node.direct
  for glyph in direct.traverse_glyph(direct.todirect(head)) do
    if is_unifraktur(glyph) then
      direct.unprotect_glyph(glyph)
    end
  end
  return head
end

for _, callback_name in ipairs {'pre_linebreak_filter', 'hpack_filter'} do
  local ltj_priority =
    luatexbase.priority_in_callback(callback_name, 'ltj.main')
  local shaper_priority =
    luatexbase.priority_in_callback(callback_name, 'luaotfload.node_processor')

  if ltj_priority and shaper_priority then
    luatexbase.add_to_callback(
      callback_name, replace_unifraktur_wordmarks,
      'attisch.unifraktur_wordmarks', ltj_priority + 1)
    local shaper = luatexbase.remove_from_callback(
      callback_name, 'luaotfload.node_processor')
    luatexbase.add_to_callback(
      callback_name, shaper, 'luaotfload.node_processor', shaper_priority)
    luatexbase.add_to_callback(
      callback_name, unprotect_unifraktur,
      'attisch.unprotect_unifraktur', ltj_priority + 2)
    luatexbase.add_to_callback(
      callback_name, shaper,
      'attisch.reshape_unifraktur', ltj_priority + 3)
  end

  local letterspace_priority =
    luatexbase.priority_in_callback(callback_name, 'luaotfload.letterspace')
  if ltj_priority and letterspace_priority then
    local letterspace = luatexbase.remove_from_callback(
      callback_name, 'luaotfload.letterspace')
    luatexbase.add_to_callback(
      callback_name, letterspace,
      'luaotfload.letterspace', ltj_priority + 4)
  end
end
