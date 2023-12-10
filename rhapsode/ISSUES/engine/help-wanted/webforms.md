# Webforms

The core browser engine would deal in terms of loosely-defined links, but
sometimes pages require more free-form input.

For this reason Rhapsode would require a mode to deal with webforms, which needs
to be designed and implemented. Or maybe there'd be two modes, the second using
extended markup to deliver a more lenient and conversational experience. You'd
enter this mode by treating the <form> tag like a link.

I would need to build upon this to bookmark webpages or search the Web.

## UX

The experience I'm currently thinking about delivering for webforms, would step
you through each non-prefilled input prompting you to fill it.

For each it'll announce the label and, more quietly, the input type possibly
with brief instructions for filling it in. If it's a multichoice input, the
options would be read out as well. Your input would be interpreted according
to the input type. A pause or left/down arrow key would store that value and
move on to the next input, or right/up would return to the previous form input.

If it's a multichoice or <datalist>-associated input, it may then clarify which
you mean in exactly the same way Rhapsode normally deals with links.

Once the form has been filled in, it'll read everything back out to you again
(include any "hidden" inputs). At which point you can:

* reenter any input by specifying it's label,
* assign shorthand names (to be resolved via the multichoice handling) for any values you've entered using the "petname" command,
* or submit the form using the "submit" command.

## Conversational forms (HTML extension)

As an extension on that form-filling experience that allows websites to cater
specifically to Rhapsode's voice interface, we could add a Rhapsode-specific
property onto `<form>` elements linking to a list of newline-seperated natural
phrases that can be used for filling in the form.

"Placeholders" would occur within each phrase identifying form inputs by `name`
attribute.

To use an Alexa-derived example, this list could start with:

    I want a {pet}.
    I want a {size} {pet}.
    I want a {pet} {energy}.
    I want a {pet} {temperament}.
    I want a {temperament} {pet}.
    I want a {size} {temperament} {pet}.
    I want a {size} {temperament} {pet} {energy} at my apartment.

Which would enhance interaction with:

    <form ... rhaps-utterances-src="...">
      <dl>
        <dt><label for="input-pet">What type of pet do you want?</label></dt>
        <dd><select id="input-pet" name="pet">
          <option value="cat">Cat</option>
          <option value="dog" checked>Dog</option>
        </select></dd>

        <dt><label for="input-size">How big should it be?</label></dt>
        <dd><select id="input-size" name="size" required>
          <option value="xs" title="petite, itty bitty">Tiny</option>
          <option value="s" title="little, take on a plane">Small</option>
          <option value="m" title="average, up to my knees">Medium</option>
          <option value="l" title="huge, waste-high">Large</option>
        </select></dd>

        <dt><label for="input-energy" required>How energetic?</label></dt>
        <dd><select id="input-energy" name="energy">
          <option value="low" title="to relax with, to cuddle with">Low</option>
          <option value="med" title="fun to play with, plays tug of war">Medium</option>
          <option value="high" title="energetic, plays frizbee, that I can run with">High</option>
        </select></dd>

        <dt><label for="input-temperament">Family pet or guard dog?</label>
        <dd><select id="input-temperament" name="temperament">
          <option value="family" title="to relax with, to cuddle with">Family</option>
          <option value="guard" title="fun to play with, plays tug of war">Guard</option>
        </select></dd>
      </dl>
    </form>

## Form activation

To activate this mode, I'm thinking of letting the autofocus attribute trigger it
and listening for the text of the "submit" button. If there is no submit button,
I'd instead listen for "enter/submit/fill-in form".

When the page is read aloud, you will hear just the submit buttons. And with them,
the audio cue.

Alternatively pages can link to a page with a `<form>` element as the root, which
will more naturally the semantics Rhapsode applies.

## Implementation
The big question here is where to split the responsibility between the Haskell
with it's access to the HTML, and C code with it's access to the text-to-speech
& speech-to-text engines. And the secondary question of how it fits in with the
rendering pipeline.

I'll propose that the solution would be to design a new format to represent forms,
that more closely describes what data is expected rather than which OS widgets to
use. Though I would want to expose the latter so users can open standard OS
dialogs for things like colours & files.

Then Haskell can focus on converting back and forth between this format & HTML
forms, whilst C can focus on filling them in.

### Intermediate Form Representation
Each input in the form should specify:

* Whether it's required
* Name
* Label
* Default value, if any
* Whether it accepts free-form text
* Valid/favoured words/ngrams
* Editable?
* Any prescribed options, with their human friendly labels & machine friendly values.
* Whether it accepts multiple inputs.
* The input `type`, in case that matters for the embedding GUI.

### Integration
Prior to rendering a page, C will first check to see if there's any forms for it
to process.

If so it'll step the user through it and use those results instead to render the
output SSML and link tables. Those links would be handled specially to refer to
specific inputs, ultimately calling a special `fetch` Haskell function to attach
the form data.

## How you can help?
You could:

* Get started on either the C or Haskell side,
* Or it'd be helpful just to help me figure out how all the form inputs/elements
  translate over to the fields listed above.
