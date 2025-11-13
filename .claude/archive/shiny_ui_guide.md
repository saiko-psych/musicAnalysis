# Shiny UI Design Guide for musicAnalysis

## Overview
This guide explains how to create attractive, functional UI elements in Shiny apps.

## Navigation Between Tabs

### Problem
The onclick handlers weren't working because we used incorrect CSS selectors.

### Solution
Shiny tabs use `data-value` attributes. To switch tabs programmatically:

```r
# CORRECT way to make clickable panels that switch tabs:
wellPanel(
  style = "cursor: pointer;",
  onclick = "Shiny.setInputValue('switch_tab', 'klawa', {priority: 'event'});",
  # content here
)

# Then in server:
observeEvent(input$switch_tab, {
  updateTabsetPanel(session, "main_tabs", selected = input$switch_tab)
})
```

### Alternative (Pure JavaScript)
```javascript
onclick = "$('#main_tabs').find('a[data-value=\"klawa\"]').tab('show');"
```

## Collapsible Sections

### Using HTML `<details>` and `<summary>`
```r
tags$details(
  tags$summary(
    style = "cursor: pointer; padding: 10px; background-color: #f0f0f0;",
    tags$strong("Click to expand")
  ),
  tags$div(
    style = "padding: 10px;",
    "Hidden content here"
  )
)
```

### Using Shiny conditionalPanel (reactive)
```r
# UI
actionButton("toggle_history", "Show/Hide Version History"),
conditionalPanel(
  condition = "input.show_history",
  # content here
)

# Server
observeEvent(input$toggle_history, {
  updateCheckboxInput(session, "show_history", value = !input$show_history)
})
```

## Styling Tips

### Colors and Backgrounds
```r
# Green success box
style = "background-color: #e8f5e9; border-left: 4px solid #4caf50;"

# Blue info box
style = "background-color: #e3f2fd; border-left: 4px solid #2196f3;"

# Orange warning box
style = "background-color: #fff3e0; border-left: 4px solid #ff9800;"

# Gray neutral box
style = "background-color: #f5f5f5; border-left: 4px solid #9e9e9e;"
```

### Hover Effects
```r
style = "cursor: pointer; transition: background-color 0.3s;"
onmouseover = "this.style.backgroundColor='#f0f0f0';"
onmouseout = "this.style.backgroundColor='white';"
```

### Icons
Use Unicode emoji (works everywhere):
- 🎵 Music (\U0001F3B5)
- 🎼 Score (\U0001F3BC)
- 🔗 Link (\U0001F517)
- ✅ Checkmark (\U00002705)
- ❌ Cross (\U0000274C)
- 📊 Chart (\U0001F4CA)
- 🔍 Magnifying glass (\U0001F50D)

## Full Example: Clickable Module Cards

```r
# UI
wellPanel(
  id = "klawa_card",
  style = "background-color: #e8f5e9; cursor: pointer; transition: all 0.3s;",
  onclick = "Shiny.setInputValue('navigate_to', 'klawa', {priority: 'event'});",
  onmouseover = "this.style.transform='scale(1.02)'; this.style.boxShadow='0 4px 8px rgba(0,0,0,0.2)';",
  onmouseout = "this.style.transform='scale(1)'; this.style.boxShadow='none';",
  tags$h5("🎵 KLAWA Module"),
  tags$p("Click to open")
)

# Server
observeEvent(input$navigate_to, {
  updateTabsetPanel(session, "main_tabs", selected = input$navigate_to)
})
```

## Best Practices

1. **Always use CSS for styling**, not inline attributes when possible
2. **Use `tags$` functions** for HTML elements
3. **Test interactivity** - hover, click, focus states
4. **Mobile-friendly** - use relative units (%, em) not fixed px
5. **Accessibility** - add proper labels and ARIA attributes
6. **Consistent spacing** - use padding and margins uniformly

## Common Pitfalls

❌ **Wrong**: `onclick = "document.querySelector('a[data-value=\"klawa\"]').click();"`
✅ **Right**: Use Shiny's input system or Bootstrap's `.tab('show')`

❌ **Wrong**: Hardcoded colors everywhere
✅ **Right**: Define color schemes at the top

❌ **Wrong**: JavaScript that modifies session state directly
✅ **Right**: Use `Shiny.setInputValue()` to trigger reactive changes
