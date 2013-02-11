template = Handlebars.compile("""
  <div class="char-info">
    <form>
      <div class="char-info-display">
        <span>{{{view.moe}}}</span>
        <span>{{view.display}}</span>
        <span>{{{view.exact}}}</span>
      </div>
      <input type="text" name="cns" value="{{exact.cns}}"></input>
      <input type="text" name="comments" value="{{comments}}"></input>
    </form>
  </div>
""")

onReady = ->
  $context = $('#context')

  $.getJSON('/api/chars/all', (data) ->
    for key, datum of data
      view = {}

      # TODO: Select display from multiple sources pua -> uni -> ids
      view.display = datum.display.uni

      match = /^moe:revised\/([0-9a-f]+)$/.exec(key)
      if match?
        view.moe = "<img src=\"http://140.111.34.46/dict/fonts/#{match[1]}.gif\">"

      match = /^(\d+)-([0-9A-F]+$)/.exec(datum.exact.cns)
      if match?
        view.exact = "<img src=\"http://www.cns11643.gov.tw/MAIDB/png.do?page=#{match[1]}&code=#{match[2]}\">"

      datum.view = view

    for key, datum of data
      html = template(datum)
      $context.append(html)
  )

$(onReady)
