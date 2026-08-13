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

local function unprotect_unifraktur(head)
  local direct = node.direct
  for glyph in direct.traverse_glyph(direct.todirect(head)) do
    local data = font.getfont(direct.getfont(glyph))
    if data and data.fullname == 'UnifrakturMaguntia' then
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
    local shaper = luatexbase.remove_from_callback(
      callback_name, 'luaotfload.node_processor')
    luatexbase.add_to_callback(
      callback_name, shaper, 'luaotfload.node_processor', shaper_priority)
    luatexbase.add_to_callback(
      callback_name, unprotect_unifraktur,
      'attisch.unprotect_unifraktur', ltj_priority + 1)
    luatexbase.add_to_callback(
      callback_name, shaper,
      'attisch.reshape_unifraktur', ltj_priority + 2)
  end

  local letterspace_priority =
    luatexbase.priority_in_callback(callback_name, 'luaotfload.letterspace')
  if ltj_priority and letterspace_priority then
    local letterspace = luatexbase.remove_from_callback(
      callback_name, 'luaotfload.letterspace')
    luatexbase.add_to_callback(
      callback_name, letterspace,
      'luaotfload.letterspace', ltj_priority + 3)
  end
end
