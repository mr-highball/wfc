# Project-owned streaming SHA-256

SPDX-License-Identifier: MIT

`tools/wfc_sha256.pas` implements byte-oriented SHA-256 in project-owned
Pascal for asset identity. Its only production dependency is `SysUtils`.
It contains no copied reference C implementation or external cryptographic
library. It is a tool unit, not a dependency of WFC's core/runtime package.
See [asset checking](assets.md) for its native filesystem consumer.

## Interface

```pascal
type
  EWfcSha256 = class(Exception);
  TWfcSha256Bytes = array of Byte;
  TWfcSha256Digest = array[0..31] of Byte;
  TWfcSha256BitLength = record
    High, Low: Cardinal;
  end;

  TWfcSha256Context = class
  public
    constructor Create;
    procedure Reset;
    procedure Update(const ABytes: TWfcSha256Bytes); overload;
    procedure Update(const ABytes: TWfcSha256Bytes;
      const AOffset, ACount: Integer); overload;
    function Finish: TWfcSha256Digest;
    function CopyBitLength: TWfcSha256BitLength;
    property Finalized: Boolean read FFinalized;
  end;

function WfcSha256AddByteLength(const ALength: TWfcSha256BitLength;
  const AByteCount: Cardinal): TWfcSha256BitLength;
function CalculateWfcSha256(const ABytes: TWfcSha256Bytes): TWfcSha256Digest;
function WfcSha256DigestHex(const ADigest: TWfcSha256Digest): String;
```

This declaration excerpt omits private fields, including `FFinalized`.
Callers own and free contexts. Independent contexts have no shared mutable
state; concurrent access or destruction of the same context requires caller
synchronization.

`Create` and `Reset` start an empty message. `Update` appends the selected
bytes without retaining the caller's buffer. Offset/count are zero-based,
nonnegative and checked using subtraction before addition. An empty update
is valid at any offset in `[0, Length(ABytes)]` while the context is open.
`Finish` returns a detached digest and closes the context. Any later update,
including an empty one, or a second `Finish` raises `EWfcSha256` until `Reset`.
Reset also deliberately discards an unfinished message.

The copied bit count remains the original message length after `Finish`,
excluding padding; it cannot set or resume compression state. Hex output is
exactly 64 uppercase ASCII hexadecimal characters with no prefix, whitespace
or newline. There is no text conversion, stream/file access, seek, extraction,
retry, digest import or state serialization in this unit. Native consumers
can reuse one byte buffer and pass the actual read count to the ranged
overload; a final partial read must not hash the unused tail.

## Checked arithmetic and capacity

Compression words are `Cardinal`. Modulo-2^32 addition splits operands into
16-bit lanes: each intermediate sum is at most 131071, and the high lane is
masked before reconstruction. The result remains within `High(Cardinal)`.
No arithmetic overflow is performed and then masked away; range/overflow
checks are not disabled. No wrapping UInt64 addition or floating 64-bit sum
is used. Logical shifts and rotations follow 32-bit Pascal word semantics,
with rotation counts in 1..31. The 64-word schedule and eight working words
use bounded local storage.

The length counter stores bits as separate high and low 32-bit words. Its
pure helper splits multiplication by eight into lanes, detects low-word
carry and checks the high addition before returning. A non-byte-aligned
starting length or sum at or beyond 2^64 bits is rejected. The helper tests
carry and overflow without processing gigabytes and does not expose a way
to change the context's actual counter.

Maximum byte-oriented message length is **2^61 - 1 bytes**, an intrinsic
SHA-256 bound, not a configurable demo limit. The asset manifest can encode
larger decimal sizes up to its separate signed 64-bit range; the native
checker explicitly refuses unsupported actual hash input. Individual
updates also obey the host's dynamic-array and `Integer` bounds. There is
no smaller whole-message cap; processing time remains proportional to bytes.

Expected validation failures precede mutation. Allocation/runtime failure,
forged objects, use-after-free and concurrent mutation are outside this
typed synchronous ownership contract. This is not persistent atomic storage
or a hardened secrets container. Padding uses the original big-endian bit
length without changing the reported message length.

## JavaScript boundary

Ordinary pas2js dynamic Byte arrays are supported; arbitrary typed arrays
are not silently converted, and raw `null` is not an empty Pascal array.
The selected range must contain dense own data properties holding finite
integer bytes. Passive descriptor checks precede indexed reads: ordinary
sparse, inherited-slot, accessor and coercion inputs raise `EWfcSha256`.
Unselected slots are deliberately not inspected. Digest and bit-length
inputs have explicit shape/range checks too.

The implementation does not call caller-provided `slice`, iterator or numeric
methods. Frozen arrays and harmless shadowed methods remain usable. It does
not claim isolation from proxies, forged owners, hostile replacement of
global intrinsics or shared-memory mutation.

## Verification

`test/wfc_sha256_test.lpr` covers:

- Published empty, `abc`, 56-byte, million-`a`, ten-block, single binary-byte,
  16-byte binary and 163-byte binary vectors, including zero bytes.
- Every two-part split and chunk sizes 1..67 for lengths
  0, 1, 2, 54, 55, 56, 57, 63, 64, 65, 119, 120, 127, 128 and 129.
  These compare with one-shot output: they are metamorphic checks, not
  independently sourced golden digests.
- Range refusals, no mutation on expected errors, finalized-state refusal,
  reset of open/closed contexts, independent contexts and detached results.
- Exact counter lanes around 2^32 bits, four GiB, the maximum byte-aligned
  length, low carry/high addition, and rejection of overflow/non-byte lengths.
- Actual-browser malformed containers, bytes, counts and digests, refusal
  without getter/coercion execution, frozen arrays and shadowed methods.

The initial focused validation on 2026-09-07 UTC passed **2131 checks on each
of stable/development FPC Win32 and Win64**, using
`-B -Mdelphi -Sa -Cr -Co -Ci -gl`. The same source suite passed **2225 checks
in an actual pas2js browser**, served and observed through the included FPC
tools. The extra browser assertions cover the raw JavaScript boundary; the
different totals are not a native/browser output-parity experiment. Neither
large-counter unit tests nor these finite message vectors demonstrate that
a maximum-sized message was actually streamed. No throughput claim follows
from these correctness checks.

The portable test is registered in the maintained native and browser gates.
Current compiler/browser results must still be checked for the revision being
released; this dated focused evidence does not certify future changes.

## Specification and vector sources

- [NIST FIPS 180-4 publication](https://csrc.nist.gov/pubs/fips/180-4/upd1/final)
  identifies the Secure Hash Standard and SHA-256.
- [RFC 6234, sections 3–6 and 8.5](https://www.rfc-editor.org/rfc/rfc6234.html)
  gives mathematical operations, constants, padding and byte-oriented test
  vectors. This Pascal unit follows the algorithm definitions, not a
  translation of its separately licensed reference C implementation.
- [NIST SHA-256 worked examples](https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-and-Guidelines/documents/examples/SHA256.pdf)
  give the `abc` and 56-byte message digests and compression blocks.
- [RFC 8448, section 3](https://www.rfc-editor.org/rfc/rfc8448.txt)
  records the empty-transcript SHA-256 digest.
- [NIST secure hashing validation information](https://csrc.nist.gov/Projects/Cryptographic-Algorithm-Validation-Program/Secure-Hashing)
  describes a separate formal validation program. These project tests are
  not CAVP validation or FIPS certification.

Hash agreement checks byte identity against a declaration. It does not
authenticate that declaration or prove asset authorship, redistribution
permission, collision impossibility, or legal compatibility. This unit is
not an HMAC, password hash or complete cryptographic protocol.
