export interface OffsetLine {
  line: string;
  /** Byte offset of the line's first byte in the whole file. */
  offset: number;
}

/**
 * Splits a byte stream on "\n". It works on bytes rather than decoded text
 * so offsets count bytes (what a ranged read takes) and a multi-byte
 * character split across chunks is decoded whole.
 */
export class LineSplitter {
  private pending: Buffer = Buffer.alloc(0);
  private pendingOffset: number;

  constructor(startOffset = 0) {
    this.pendingOffset = startOffset;
  }

  push(chunk: Buffer | string): OffsetLine[] {
    const bytes = Buffer.isBuffer(chunk) ? chunk : Buffer.from(chunk);
    const buf = this.pending.length
      ? Buffer.concat([this.pending, bytes])
      : bytes;
    const lines: OffsetLine[] = [];
    let start = 0;
    for (let nl = buf.indexOf(0x0a); nl !== -1; nl = buf.indexOf(0x0a, start)) {
      lines.push({
        line: buf.toString("utf-8", start, nl),
        offset: this.pendingOffset + start,
      });
      start = nl + 1;
    }
    this.pending = buf.subarray(start);
    this.pendingOffset += start;
    return lines;
  }

  /** What follows the last newline, if anything. */
  rest(): OffsetLine | null {
    if (!this.pending.length) return null;
    return {
      line: this.pending.toString("utf-8"),
      offset: this.pendingOffset,
    };
  }
}
