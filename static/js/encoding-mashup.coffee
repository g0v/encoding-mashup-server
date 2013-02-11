template = Handlebars.compile("""
  <div class="char-info">
    <form data-char-id="{{view.id}}">
      <div class="char-info-display">
        <span>{{{view.moe}}}</span>
        <span>{{view.display}}</span>
        <span>{{{view.exact}}}</span>
      </div>
      <input type="text" name="cns" value="{{exact.cns}}"></input>
      <input type="text" name="comments" value="{{comments}}"></input>
      <button class="btn btn-small btn-primary" type="button">Save</button>
    </form>
  </div>
""")

allCharData = {}

onReady = ->
  $context = $('#context')

  $.getJSON('/api/chars/all', (data) ->
    console.dir data

    # Bind to this file's scope
    allCharData = data

    for key, datum of data
      view = {}

      view.id = key
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

    $('button').on('click', (event) ->
      $form = $(this).parent()
      id = $form.data('char-id')
      datum = allCharData[id]

      # Tricky deep clone
      datum = JSON.parse(JSON.stringify(datum))

      cns = $form.children('input[name="cns"]').val()
      comments = $form.children('input[name="comments"]').val()

      datum.comments = comments
      datum.exact.cns = cns

      url = "/api/char/#{encodeURIComponent(datum.view.id)}"

      # TODO: Refactoring: Move extra info from `view` to somewhere else
      delete datum.view

      $.ajax {
        url: url
        type: 'PUT'
        data: JSON.stringify(datum)
      }

    )
  )

$(onReady)
