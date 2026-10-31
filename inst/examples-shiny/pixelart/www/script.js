$(function() {
  let color = '#000000';
  let drawing = false;
  let stroke = [];
  const $grid = $('#pa-grid');

  const tool = function() {
    return $('input[name=tool]:checked').val();
  };

  // Erasing paints white
  const paintColor = function() {
    return tool() === 'erase' ? '#FFFFFF' : color;
  };

  const paintCell = function($c) {
    const i = $c.data('i');
    if (stroke.includes(i)) return;
    stroke.push(i);
    if (tool() !== 'fill') $c.css('background', paintColor());
  };

  const finish = function() {
    drawing = false;
    if (!stroke.length) return;
    Shiny.setInputValue('stroke',
      {cells: stroke, tool: tool(), color: paintColor()},
      {priority: 'event'}
    );
    stroke = [];
  };

  $grid.on('pointerdown', '.pa-cell', function(e) {
    e.preventDefault();
    // touch pointers are captured by the first cell; release so drags reach other cells
    const pid = e.originalEvent.pointerId;
    if (e.target.hasPointerCapture(pid)) e.target.releasePointerCapture(pid);
    drawing = true;
    stroke = [];
    paintCell($(this));
    if (tool() === 'fill') {
      finish();
    }
  });
  $grid.on('pointerover', '.pa-cell', function() {
    if (drawing && tool() !== 'fill') {
      paintCell($(this));
    }
  });
  $(document).on('pointerup', function() {
    if (drawing) {
      finish();
    }
  });

  const setColor = function(c) {
    color = c;
    $('.pa-swatch').removeClass('active')
      .filter(function() { return $(this).data('color') === c; }).addClass('active');
  };

  $('.pa-swatch').on('click', function() {
    setColor($(this).data('color'));
  });
  setColor(color);

  // Let the CSS know which tool is active so it can change the cursor
  const setTool = function() {
    $grid.attr('data-tool', tool());
  };
  $('input[name=tool]').on('change', setTool);
  setTool();

  // Clicking a dot in the history timeline jumps to that step. The dots are
  // re-created whenever the history changes, so listen on the document
  $(document).on('click', '.pa-dot', function() {
    Shiny.setInputValue('jump', $(this).data('step'), {priority: 'event'});
  });

  // Keep the newest entry of the code log in view
  const log = document.getElementById('log');
  new MutationObserver(function() { log.scrollTop = log.scrollHeight; })
    .observe(log, {childList: true, characterData: true, subtree: true});

  Shiny.addCustomMessageHandler('pa-render', function(msg) {
    $grid.children().each(function(i) { $(this).css('background', msg.cells[i]); });
    $('#undo').prop('disabled', !msg.can_undo);
    $('#redo').prop('disabled', !msg.can_redo);
    $('#start').prop('disabled', !msg.can_undo);
    $('#latest').prop('disabled', !msg.can_redo);
    $('#clear').prop('disabled', !msg.can_undo && !msg.can_redo);
  });
});
