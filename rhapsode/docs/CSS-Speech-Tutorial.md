**Wanted:** Guidance on creating a great audio theme. Maybe based on voice acting
or public speaking theory.

Rhapsode still lets you apply CSS styles to your webpages, but since it outputs
audio rather than video it supports a different set of CSS properties. This page
provides an overview of these properties.

## Should it be spoken?
You can use the `speak` property to determine whether an HTML element should be
read aloud or not, and the `speak-as` property to determine how it reads digits
and/or punctuation.

Setting `speak: never` is not the same as setting `voice-volume: silent` as the
latter still takes up the same ammount of time as it would've to read the text
aloud.

### Reference:
#### `speak`
Usage:
  `speak: always`

Valid Options:
  - `always` - Yes, it should be read aloud. **default**
  - `never` - Skips over the element entirely, no time spent on it.

#### `speak-as`
Usage:
  `speak-as: spell-out`

Valid options:
  - `spell-out` - announces each individual letter or digit.
  - `digits` - announces each individual digit.
  - `literal-punctuation` - announces any punctuation marks.
  - `no-punctuation` - Does not read any punctuation.

## The Voice
You can use the `voice-family` attribute to select a voice either by age/gender/variant
or by it's name. Just like font-family this'll make a big difference to the "look"
of your page.

### Reference
#### `voice-family`
Usage:
  `voice-family: child female`

Specifies a comma-seperated list of voices, which can be specified by name (known
to the text-to-speech engine) or age/gender/variant-number which it depicts.
age/gender/number must be specified in that order, but only gender must be specified.

- genders: `male` `female` `neutral`
- ages: `child` `young` `old`. May also be specified as approximate age in years.


## Speaking Style
You can alter the voice you choose by varying it's volume, rate, pitch, range,
and stress. Doing so helps people pay attention, especially if it reinforces the
meaning of your text.

### Keywords & Offsets
All the speaking style properties provide keywords you can use instead of a
number. In which case write a number after a keyword to represent an offset from
that keyword.

### Reference
#### `voice-volume`
How loud the sound is, either as keyword or in decibels.

Usage:
  `voice-volume: medium -10dB`

Valid keywords:
  - `silent` - no sound for this duration.
  - `x-soft` - quieter
  - `soft`
  - `medium`
  - `loud`
  - `x-loud` - louder

#### `voice-rate`
How fast the sound is, as a keyword with optional percentage adjustment.

Usage:
  `voice-rate: medium -10%`

Valid keywords:
  - `x-slow` - slower
  - `slow`
  - `medium`
  - `fast`
  - `x-fast` - faster

#### `voice-pitch`
How high or low the sound is, in (kilo)hertz or with percentage/semitone adjustment.

Usage:
  `voice-pitch: medium +10%`

Valid keywords:
  - `x-low` - lower pitch
  - `low`
  - `medium`
  - `high`
  - `x-high` - higher pitch
  - `absolute` - indicates the given value in (kilo)hertz should be used regardless of the inherited value.

#### `voice-range`
By how much does the pitch of the sound change. Syntax matches `voice-pitch`.

#### `voice-stress`
Usage:
  `voice-stress: moderate`

Valid keywords:
  - `strong` - more stress
  - `moderate`
  - `none`
  - `reduced` - less stress.

## Rests, Pauses, and Auditory Cues
On either end of your text you can place an audio cue to identify it, and on
either end of those you can insert additional silence. If two silences are
directly adjacent, the smaller one will be removed.

The inner pauses are called the element's `rest` and the outer ones are called
it's `pause`.

The user agent stylesheet, for example, uses audio cues to indicate list bullets
and links. And silence functions exactly like whitespace in a visual browser.


### Reference
> Note: `pause` and `rest` mirror functionality
> Note: `-before` and `-after` are both offered by the api as well
#### `pause`/`rest`
Usage:
  `pause-before: weak 50ms`

In seconds or milliseconds, or as keyword.

Valid keywords:
  - `x-weak` - less
  - `weak`
  - `medium`
  - `strong` - more

#### `cue`
Usage:
  `cue-after: url(censor.wav)`

Specifies .wav file to play between the corresponding pause/rest by relative URL.

## Text Generation
Rhapsode supports (some of) the same text generation attributes as visual
browsers, namely:

* `counter-reset`
* `counter-increment`
* `counter-set`
* `content`

Though more may be added in the future.

However unlike visual browsers you can apply the `content` property to the element
itself to replace it's own children.

---

* [CSS3 Speech Module](https://drafts.csswg.org/css-speech-1/) (Retired W3C Note)
* [MDN on CSS Counters](https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Lists_and_Counters/Using_CSS_counters)
