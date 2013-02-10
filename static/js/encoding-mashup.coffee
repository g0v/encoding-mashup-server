template = Handlebars.compile("""
  <div class="char-info">
    <div class="char-info-display">
      <span>{{display.uni}}</span>
    </div>
     <input type="text" name="cns" value="{{exact.cns}}">
  </div>
""")

onReady = ->

  $context = $('#context')

  $.getJSON('/api/chars/all', (data) ->
    for k, v of data
      html = template(v)
      $context.append(html)
  )

$(onReady)
