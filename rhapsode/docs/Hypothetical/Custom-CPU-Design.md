This page describes some hypothetical hardware designed specifically to run a
Rhapsode-like web browser. There are no plans to build *this* hardware.

However this hypothetical may help to clarify how Rhapsode works.

## The Task
A navigation task is performed by:

1. Performing voice recognition
2. Matching the textual translations to links on the page
3. Parse the (relative) URL
4. Lookup URL in cache
5. Resolve the domain name via DNS if needed
6. Send a HTTP(/TLS)/TCP/IP network request ideally on a previously open connection
7. Parse the HTTP response, having dispatched network input to the right thread
8. Parse the HTML to extract CSS & links
9. Update the language model for voice recognition
10. Parse the CSS
11. Convert the HTML to SSML via CSS
12. Convert the SSML text to phonemes via natural language-specific rules
13. Convert the SSML/phonemes into audio manipulations
14. Output raw audio

Almost all of those steps are straightforward format conversions (possibly via
instructions extracted from HTML) or map lookups. So that's what I'll design here.

The main exceptions are TLS, TCP, & especially voice recognition. TLS requires
circuitry that can perform en/de-cryption. Whilst TCP requires cancellable
timeouts, randomness, and coroutines. Voice recognition will be addressed later.

## Parsing
Let's say network, buttons, and (voice-recognized) audio is written into a
ringbuffer  by those input devices. Each 4bits(?) of which would navigate a
graph describing the syntax being matched.

The nodes in that graph could "call" other syntaxes or tries (for more complex
syntaxes) before "returning" to where it left off by pushing and popping a stack.

If the parsing CPU encounters a node that's not in it's cache memory (a
"cache miss"), I'd have it immediately load it in from memory. And since
this CPU focuses on format conversions anyways, it could be repurposed to
decompress/decode the new instructions.

## Reformatting
Each of the parsing rules you can call could optionally have a corresponding
instructions for what to do upon pop. So that those instructions could be
prefetched upon push and enqueued upon pop. There may also be an "echo"
shorthand in this process.

Those instructions in turn would output bytes to external hardware, cached disk
pages, and/or other programs as tracked in a "capabilities stack". Bytes written
to other programs would be queued up in an "idle" ringbuffer to be dequeued when
there's no external input.

To compile machine code, update caches, add a timeout, or sort/dedup output
there'd also need to be an instruction to rewrite specified page(s) of memory.
This could be handled using the same circuits as cache misses during parsing,
or it could trigger the interrupt only once the fetch has completed.

---

Occasionally an ALU would be required for encryption, comparison, checksums,
sound effects, etc.

Coroutines would be required for TLS and (navigatable) audio output. Saves could
be done by writing a pointer to it's stack(s) to another page. And restores could
occur via parsing cache miss once it's been looked up.

## Memory Blocks
This hypothetical CPU would require very little circuitry, relying almost entirely
on multiple independant chunks of memory that can be accessed concurrently. It
should be trivial to build on a FPGA.

Specially it'd include memory blocks for:

1. Input queue
2. Parsing graph (split in 2?)
3. Parsing stack
4. Prefetch stack
5. Output instruction queue
6. Capabilities stack
7. Staging areas/stacks
8. Idle queue

There may be a second core that turns on when the idle queue overflows, which
would have (some of) it's own dedicated memory blocks. Also a bitmask could be
used for allocate overflow and other pages.

Furthermore the queues and stacks could have near-perfect cache hit rates, and
would rarely overflow to memory.

## Voice Recognition
There are two approaches to voice recognition I'm familiar with: Mozilla Deep
Voice & CMU Sphinx. What's described here caters to both approaches, whilst the
circuit described above caters to neither.

No one understands how any specific neural network (like Mozilla Deep Voice)
works, but I can expand upon how CMU Sphinx works:

1. Compute "feature vectors" to describe each sliver of audio.
2. Use "Hidden Markov Models" (HMMs) to convert those feature vectors to "phones".
3. Use a "language model" (ngrams or finite-state automatons) to convert phones into possible texts.

### Circuitry

Hidden Markov Models, finite-state automatons, & ngram models can all be viewed
as variants of a probability graph. To traverse these we need extensive
multiplication, addition, and random-access memory lookups.

Additions and multiplications can be combined into a matrix multiplication
operation, which are also heavily used in neural networks and optimized for by
GPU & KPU hardware. Maybe this could be reused to perform the audio analysis?

Random-access memory lookups meanwhile require some sort of RAM which is hard to
optimize. But because having too large (or too small) of a language model gives
a bad UX, it would be appropriate to heavily limit the available memory. And make
heavier use of matrix multiplies then CMU Sphinx might.

This happens to closely describe the [MAIX SoC](https://www.seeedstudio.com/sipeed)
which may power some real Rhapsode hardware.
