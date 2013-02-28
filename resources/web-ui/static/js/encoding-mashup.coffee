template = Handlebars.compile("""
  <div class="char-info">
    <form data-char-id="{{view.id}}">
      <div class="char-info-display">
        <span>{{{view.moe}}}</span>
        <span>{{view.display}}</span>
        <span>{{{view.exact}}}</span>
      </div>
      <input type="text" name="cns" value="{{exact.cns}}"></input>
      <input type="text" name="comment" value="{{comment}}"></input>
      <button class="btn btn-small btn-primary" type="button">儲存</button>
      <span class="saved" style="display: none">已儲存！</span>
    </form>
  </div>
""")

allCharData = {}

onReady = ->
  $context = $('#context')
  $navbar = $('#navbar')
  $load_progress_bar = $('#load-progress-bar')
  $load_progress_bar_container = $('#load-progress-bar-container')

  #FIXME: Fake progress bar
  $load_progress_bar.width('100%').text('100%');

  $.getJSON('/api/chars/all', (data) ->
    # Bind to this file's scope
    allCharData = data.charmap

    for key, datum of data.charmap
      view = {}

      view.id = key
      # TODO: Select display from multiple sources pua -> uni -> ids
      view.display = datum.display.uni

      match = /^moe\/revised\/([0-9a-f]+)$/.exec(key)
      view.moe =
        if match?
          "<img src=\"http://140.111.34.46/dict/fonts/#{match[1]}.gif\">"
        else
          '？'

      match = /^(\d+)-([0-9A-F]+$)/.exec(datum.exact.cns)
      view.exact =
        if match?
          "<img src=\"http://www.cns11643.gov.tw/MAIDB/png.do?page=#{match[1]}&code=#{match[2]}\">"
        else
          '？'

      datum.view = view

    for key, datum of data.charmap
      html = template(datum)
      $context.append(html)

    $('button').on('click', (event) ->
      $form = $(this).parent()
      id = $form.data('char-id')
      datum = allCharData[id]

      # Tricky deep clone
      datum = JSON.parse(JSON.stringify(datum))

      cns = $form.children('input[name="cns"]').val()
      comment = $form.children('input[name="comment"]').val()

      datum.comment = comment
      datum.exact.cns = cns

      url = "/api/char/#{encodeURIComponent(datum.view.id)}"

      # TODO: Refactoring: Move extra info from `view` to somewhere else
      delete datum.view

      $saved = $form.children('.saved')
      $.ajax {
        url: url
        type: 'PUT'
        data: JSON.stringify({ version: 0, charinfo: datum })
        success: ->
          $saved.show()
          $saved.fadeOut(1000)
      }

    )

    $context.show()
    $navbar.show()
    $load_progress_bar_container.hide()
  )

$(onReady)
