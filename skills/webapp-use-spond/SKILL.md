---
name: webapp-use-spond
description: Use when automating Spond event workflows — creating and editing tasks on events.
---

# /webapp-use-spond - Spond Event Task Automation

## Purpose

Automate task management on Spond events — creating, editing, and deleting tasks
from a structured task definition file.

## Prerequisites

- Edge/Chrome browser running with CDP: `--remote-debugging-port=9222`
- User logged in to spond.com (session persists in browser profile)
- Python with `playwright` — use `/python` skill if available to determine
  the correct invocation (e.g., `uvx --with playwright python -c '...'`)

## Usage

```
/webapp-use-spond add-tasks --event <EVENT_ID> --tasks <path-to-tasks.md>
```

## Task Definition Format

Tasks are defined in a markdown file with day headers and task lines:

```markdown
## Fredag

Stevnekontor 1
Sekretariat 2
Hopp - funksjonær 8
Hopp - øvelsesleder 3

## Lørdag

Kafé 5
Kule - funksjonær 3
Kule - øvelsesleder 1
```

Each line: `<task name> <number|inf>`. The number is people needed.
`inf` means unlimited volunteers.

Task names in the Spond event get a day prefix: `Lørdag - Kafé`,
`Lørdag - Kule - øvelsesleder`, `Fredag - Stevnekontor`, etc.

## Site Details

- **Event URL**: `https://spond.com/client/sponds/<EVENT_ID>`
- **Framework**: React SPA with styled-components
- **Language**: UI may be Norwegian or English depending on user settings

## Spond Interaction Rules

1. **Standard `fill()` works** for text inputs (`input[name="name"]`, `textarea[name="description"]`)

2. **Custom dropdowns** — the "Number of people needed" selector
   (`#volunteers_number_selector`) is a custom `<button>` that opens a `<ul>`
   of `<li><button>` items. Click the selector button, then click the item.

3. **Wait after every action** — `wait_for_load_state('networkidle')` +
   `wait_for_timeout(1000)` after clicks that trigger UI changes.

4. **Modal stacking** — dialogs layer on top of each other. The task form is a
   modal on top of the task list modal, which is on top of the edit event modal.
   Closing returns to the previous layer.

5. **`name` attribute selectors are NOT unique across modals** — buttons use
   `name="confirm-button"`, `name="cancel-button"`, `name="delete-button"`,
   but these same names appear in every modal layer simultaneously. The edit
   event form has a "Save" `confirm-button`, the task list has a "New task"
   `confirm-button`, and delete confirmation has an "OK" `confirm-button`.
   **Never use bare `name=` selectors for buttons. Always use
   `get_by_role("button", name="New task")` or similar text-based role
   selectors.**

## Page Map

### Event Page

- **URL**: `https://spond.com/client/sponds/<EVENT_ID>`
- **Edit button**: top-right of event card, text "Edit" / "Rediger"

### Edit Event Dialog

- **Tasks field**: between "Hosts" and "Default attendance status"
- **Add task button**: clickable area with text "Add task" / "Legg til oppgave"
- **Existing task rows**: `li` elements with class prefix `eventComposerstyled__TableViewRow`
  - Task name: `.eventComposerstyled__TableViewName-sc-oo95ot-34`
  - Description: `.eventComposerstyled__TableViewDescription-sc-oo95ot-35` (e.g., "1 person needed")
  - Edit link: `.eventComposerstyled__TableViewEdit-sc-oo95ot-36`
- **Add another task**: `button[name="event_add_another_task"]`

### Tasks List Dialog (modal)

- **Title**: "Tasks" / "Oppgaver"
- **New task button**: `get_by_role("button", name="New task")` — do NOT use
  `button[name="confirm-button"]` as it collides with the Save button behind this modal
- **Template rows**: `.taskComposerEntrystyled__TasksItem-sc-197k4pz-4`
  - Title: `.taskComposerEntrystyled__TasksItemTitle-sc-197k4pz-7`
  - Description: `.taskComposerEntrystyled__TasksItemDescription-sc-197k4pz-8`
- **Cancel**: `get_by_role("button", name="Cancel", exact=True)`

### New/Edit Task Form (modal)

| Field | Selector | Type | Notes |
|-------|----------|------|-------|
| Title | `input[name="name"]` | text | maxlength=255 |
| Description | `textarea[name="description"]` | textarea | maxlength=2047 |
| Type: Volunteers | `input[type="radio"][value="OPEN"]` | radio | Shows people-needed selector |
| Type: Assigned | `input[type="radio"][value="ASSIGNED"]` | radio | Shows assignee picker |
| People needed | `#volunteers_number_selector` | custom select | "Unlimited", 1–50. Only when type=OPEN |
| Allow children | `input[name="allowChildren"]` | checkbox | |
| Save as template | `input[name="isSaveAsTemplate"]` | checkbox | |

**Buttons:**

| Button | Selector | Notes |
|--------|----------|-------|
| Done/Save | `get_by_role("button", name="Done", exact=True)` | DOM name is "cancel-button" but use text role |
| Delete | `button[name="delete-button"]` | Only in edit mode |
| Close (X) | `span[name="close-button"]` | Top-right corner |

### People Needed Dropdown

Click `#volunteers_number_selector` to open. Items are `<li>` > `<button>` with
class `selectstyled__SelectItem-sc-ph3ydz-7`. Options: "Unlimited", then 1–50.

### Delete Confirmation

"Remove task — Are you sure you want to remove this task?"
- **OK**: `button[name="confirm-button"]` (scope to this dialog, not the task list)

## Workflows

### Add Tasks from Definition File

For each day section in the task file, for each task line:

1. Parse task name and people count from the line
2. Compose the full name as `<Day> - <Task name>` (e.g., "Lørdag - Kafé")
3. If the event has no tasks yet, click `get_by_text("Add task", exact=True)`;
   otherwise click `get_by_text("Add another task")`
4. In the Tasks dialog, click `get_by_role("button", name="New task")`
5. Fill `input[name="name"]` with the composed task name
6. Ensure radio `input[value="OPEN"]` is selected (volunteer type)
7. Click `#volunteers_number_selector`, then click `li button:text-is("<N>")`
   matching the people count (or `"Unlimited"` if `inf`)
8. Click `get_by_role("button", name="Done", exact=True)` to save
9. Back in the edit event form, repeat from step 3 for the next task

After all tasks are added, save the event.

**Wait times**: 1500ms after opening task dialog, 1000ms after clicking "New task",
500ms after dropdown interactions, 1000ms after saving. The SPA needs these pauses.

### Edit Existing Task

1. In the edit event form, find the task row by name
2. Click the "Edit" link on that row
3. Modify fields as needed
4. Click "Done" to save

### Delete Task

1. In the edit event form, find and click "Edit" on the task row
2. Click `button[name="delete-button"]`
3. Click "OK" in the confirmation dialog

## Gotchas

- **`name=` selectors are ambiguous** — `confirm-button`, `cancel-button`, and
  `delete-button` each appear in 2–3 modal layers simultaneously. Always use
  `get_by_role("button", name="<visible text>")` instead.
- **`wait_for_load_state("networkidle")` will timeout** — the SPA keeps
  connections open. Use `wait_until="domcontentloaded"` for `goto()` and
  `wait_for_timeout()` for settling after interactions.
- After adding a task, you return to the edit event form, not the task list —
  use `get_by_text("Add another task")` for subsequent tasks
- The people-needed selector only appears when task type is "OPEN" (volunteers)
- Styled-component class suffixes (hash parts) may change across deployments —
  prefer `name` and `id` selectors where possible, but for buttons always
  prefer `get_by_role()` with text
- **Standard `fill()` works** for `input[name="name"]` and `textarea[name="description"]`
- The dropdown for people count uses `li button:text-is("N")` — this is reliable
  because the dropdown is a single isolated list

## Inline Python Pattern

Always use inline scripts for automation:

```bash
uvx --with playwright python -c '
import asyncio
from playwright.async_api import async_playwright

async def main():
    async with async_playwright() as p:
        browser = await p.chromium.connect_over_cdp("http://localhost:9222")
        page = browser.contexts[0].pages[0]
        # ... automation code here

asyncio.run(main())
'
```

Never create temporary `.py` files.

## Error Handling

| Issue | Action |
|-------|--------|
| CDP connection refused | Ensure browser is running with `--remote-debugging-port=9222` |
| Not logged in (redirects to login) | Ask user to log in manually, session will persist |
| "New task" button not found | You may be in the wrong modal layer — check for task list dialog first |
| People count selector not visible | Ensure OPEN radio is selected (volunteer type) |
| Task name already exists | Spond allows duplicates — check existing tasks before adding |
| `button[name="confirm-button"]` clicks wrong thing | Never use bare `name=` selectors — use `get_by_role("button", name="<text>")` |
| `wait_for_load_state("networkidle")` hangs | SPA keeps connections open — use `wait_for_timeout()` instead |
