"use client";

import { useEffect, useRef } from "react";

const PRIMARY = { r: 17, g: 101, b: 52 };
const BRIGHT = { r: 34, g: 197, b: 94 };
const RED = { r: 239, g: 68, b: 68 };

const COLS = 80;
const ROWS = 55;

const CODE_FRAGMENTS = [
  "if rsi > 70: sell()",
  "df['sma_20'].mean()",
  "returns.std() * sqrt(252)",
  "position.close()",
  "np.mean(prices[-20:])",
  "alpha = excess / beta",
  "await engine.execute()",
  "const pnl = exit - entry",
  "if (spread > threshold)",
  "bot.onTick(price =>",
  "fn on_bar(&mut self)",
  "impl Strategy for Bot {",
  "double alpha = 0.0;",
  "decimal pnl = 0m;",
  "val signal = rsi > 70",
  "calcSharpe :: [Double] ->",
];

const TERM_FONT =
  "'SF Mono', 'Cascadia Code', 'JetBrains Mono', 'Fira Code', 'Consolas', 'Monaco', monospace";

interface FloatingCode {
  text: string;
  x: number;
  y: number;
  vx: number;
  vy: number;
  size: number;
  opacity: number;
}

interface TerrainParticle {
  nx: number;
  nz: number;
  hOffset: number;
  size: number;
  baseOpacity: number;
}

function seededRand(a: number, b: number): number {
  const s = Math.sin(a * 127.1 + b * 311.7) * 43758.5453;
  return s - Math.floor(s);
}

export function NodeNetwork() {
  const canvasRef = useRef<HTMLCanvasElement>(null);
  const animationRef = useRef<number>(0);
  const timeRef = useRef<number>(0);
  const codeRef = useRef<FloatingCode[]>([]);
  const terrainParticlesRef = useRef<TerrainParticle[]>([]);

  useEffect(() => {
    const canvas = canvasRef.current;
    if (!canvas) return;
    const ctx = canvas.getContext("2d");
    if (!ctx) return;

    let w = 0;
    let h = 0;
    const reduceMotion = window.matchMedia(
      "(prefers-reduced-motion: reduce)",
    ).matches;

    const initCode = () => {
      codeRef.current = Array.from({ length: 24 }, () => ({
        text: CODE_FRAGMENTS[Math.floor(Math.random() * CODE_FRAGMENTS.length)],
        x: Math.random() * w,
        y: Math.random() * h,
        vx: (Math.random() - 0.5) * 0.18,
        vy: (Math.random() - 0.5) * 0.12,
        size: Math.random() * 7 + 11,
        opacity: Math.random() * 0.14 + 0.06,
      }));
    };

    const initTerrainParticles = () => {
      terrainParticlesRef.current = Array.from({ length: 500 }, () => ({
        nx: Math.random(),
        nz: Math.random(),
        hOffset: Math.random() * 0.4 + 0.05,
        size: Math.random() * 1.8 + 0.3,
        baseOpacity: Math.random() * 0.4 + 0.1,
      }));
    };

    const resize = () => {
      const dpr = window.devicePixelRatio || 1;
      const rect = canvas.getBoundingClientRect();
      w = rect.width;
      h = rect.height;
      canvas.width = w * dpr;
      canvas.height = h * dpr;
      ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
      initCode();
      initTerrainParticles();
    };

    const terrain = (nx: number, nz: number, t: number): number => {
      const cx = Math.abs(nx - 0.5) * 2;
      const valley = Math.pow(cx, 1.2);
      const x = nx * Math.PI * 2;
      const z = nz * Math.PI * 2;

      let ht = 0;
      ht += Math.sin(x * 3 + t * 0.0004) * Math.cos(z * 2 + t * 0.0003);
      ht += Math.sin(x * 6 - t * 0.0008) * 0.5;
      ht += Math.cos(z * 5 + t * 0.0006) * 0.4;
      ht += Math.sin((x + z) * 4 + t * 0.001) * 0.3;
      ht += Math.sin(x * 12 + t * 0.002) * 0.15;
      ht += Math.cos(z * 10 - t * 0.0015) * 0.12;
      ht = ht * (0.06 + valley * 0.94);
      ht += valley * valley * Math.sin(z * 8 + t * 0.001) * 0.6;

      return ht;
    };

    const project = (col: number, row: number, extraH: number, t: number) => {
      const horizonY = h * 0.18;
      const centerX = w / 2;
      const nx = col / (COLS - 1);
      const nz = row / (ROWS - 1);
      const rawDepth = row / (ROWS - 1);
      const depth = Math.pow(rawDepth, 1.4);
      const screenY = horizonY + (h * 1.2 - horizonY) * depth;
      const xNorm = (col / (COLS - 1)) * 2 - 1;
      const spread = 0.4 + depth * 0.7;
      const screenX = centerX + xNorm * w * spread;
      const ht = terrain(nx, nz, t) + extraH;
      const heightPx = ht * (25 + depth * 140);

      return { x: screenX, y: screenY - heightPx, depth };
    };

    const drawCandle = (
      x: number,
      y: number,
      depth: number,
      intensity: number,
      col: number,
      row: number,
    ) => {
      const bullish = seededRand(col, row) > 0.45;
      const scale = (0.5 + depth * 0.8) * (0.7 + intensity * 0.5);
      const bodyH = (6 + intensity * 10) * scale;
      const bodyW = 3 * scale + 1.5;
      const wickH = (5 + intensity * 8) * scale;
      const alpha = intensity * (0.2 + depth * 0.5);
      const color = bullish ? BRIGHT : RED;

      ctx.beginPath();
      ctx.moveTo(x, y - wickH - bodyH / 2);
      ctx.lineTo(x, y + wickH + bodyH / 2);
      ctx.strokeStyle = `rgba(${color.r}, ${color.g}, ${color.b}, ${alpha * 0.6})`;
      ctx.lineWidth = 0.5 * scale + 0.5;
      ctx.stroke();

      ctx.fillStyle = `rgba(${color.r}, ${color.g}, ${color.b}, ${alpha})`;
      ctx.fillRect(x - bodyW / 2, y - bodyH / 2, bodyW, bodyH);
    };

    const animate = () => {
      timeRef.current += 16;
      const t = timeRef.current;
      ctx.clearRect(0, 0, w, h);

      const horizonY = h * 0.18;
      const glow = ctx.createRadialGradient(
        w / 2,
        horizonY,
        0,
        w / 2,
        horizonY,
        w * 0.6,
      );
      glow.addColorStop(
        0,
        `rgba(${PRIMARY.r}, ${PRIMARY.g}, ${PRIMARY.b}, 0.08)`,
      );
      glow.addColorStop(
        0.5,
        `rgba(${PRIMARY.r}, ${PRIMARY.g}, ${PRIMARY.b}, 0.03)`,
      );
      glow.addColorStop(1, `rgba(${PRIMARY.r}, ${PRIMARY.g}, ${PRIMARY.b}, 0)`);
      ctx.fillStyle = glow;
      ctx.fillRect(0, 0, w, h);

      const pts: { x: number; y: number; depth: number }[][] = [];
      for (let row = 0; row < ROWS; row++) {
        pts[row] = [];
        for (let col = 0; col < COLS; col++) {
          pts[row][col] = project(col, row, 0, t);
        }
      }

      for (let row = 0; row < ROWS; row++) {
        const d = pts[row][0].depth;
        ctx.strokeStyle = `rgba(${PRIMARY.r}, ${PRIMARY.g}, ${PRIMARY.b}, ${0.03 + d * 0.18})`;
        ctx.lineWidth = 0.3 + d;
        ctx.beginPath();
        for (let col = 0; col < COLS; col++) {
          const p = pts[row][col];
          if (col === 0) ctx.moveTo(p.x, p.y);
          else ctx.lineTo(p.x, p.y);
        }
        ctx.stroke();
      }

      for (let col = 0; col < COLS; col++) {
        for (let row = 0; row < ROWS - 1; row++) {
          const p1 = pts[row][col];
          const p2 = pts[row + 1][col];
          const avgD = (p1.depth + p2.depth) / 2;
          ctx.beginPath();
          ctx.moveTo(p1.x, p1.y);
          ctx.lineTo(p2.x, p2.y);
          ctx.strokeStyle = `rgba(${PRIMARY.r}, ${PRIMARY.g}, ${PRIMARY.b}, ${0.02 + avgD * 0.12})`;
          ctx.lineWidth = 0.2 + avgD * 0.7;
          ctx.stroke();
        }
      }

      for (let row = 1; row < ROWS; row += 3) {
        for (let col = 1; col < COLS; col += 3) {
          const p = pts[row][col];
          const ht = terrain(col / (COLS - 1), row / (ROWS - 1), t);
          if (ht > 0.5) {
            drawCandle(
              p.x,
              p.y,
              p.depth,
              Math.min((ht - 0.5) / 1.2, 1),
              col,
              row,
            );
          }
        }
      }

      terrainParticlesRef.current.forEach((tp) => {
        const ht = terrain(tp.nx, tp.nz, t);
        if (ht < 0.3) return;
        const p = project(
          tp.nx * (COLS - 1),
          tp.nz * (ROWS - 1),
          tp.hOffset * ht,
          t,
        );
        const intensity = Math.min((ht - 0.3) / 1, 1);
        const alpha = tp.baseOpacity * intensity * (0.2 + p.depth * 0.6);
        const size = tp.size * (0.4 + p.depth * 0.6);
        ctx.beginPath();
        ctx.arc(p.x, p.y, size, 0, Math.PI * 2);
        ctx.fillStyle = `rgba(${BRIGHT.r}, ${BRIGHT.g}, ${BRIGHT.b}, ${alpha})`;
        ctx.fill();
      });

      ctx.textBaseline = "middle";
      ctx.textAlign = "left";
      codeRef.current.forEach((c) => {
        if (!reduceMotion) {
          c.x += c.vx;
          c.y += c.vy;
          if (c.x < -300) c.x = w + 50;
          if (c.x > w + 50) c.x = -300;
          if (c.y < -30) c.y = h + 30;
          if (c.y > h + 30) c.y = -30;
        }
        ctx.font = `${c.size}px ${TERM_FONT}`;
        ctx.fillStyle = `rgba(${BRIGHT.r}, ${BRIGHT.g}, ${BRIGHT.b}, ${c.opacity})`;
        ctx.fillText(c.text, c.x, c.y);
      });

      if (!reduceMotion) {
        animationRef.current = requestAnimationFrame(animate);
      }
    };

    resize();
    animate();
    window.addEventListener("resize", resize);

    return () => {
      window.removeEventListener("resize", resize);
      cancelAnimationFrame(animationRef.current);
    };
  }, []);

  return <canvas ref={canvasRef} className="absolute inset-0 h-full w-full" />;
}
