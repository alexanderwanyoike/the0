import { renderHook, act } from "@testing-library/react";
import { useToast, reducer, toast } from "../use-toast";

describe("useToast", () => {
  describe("reducer", () => {
    const initialState = { toasts: [] };

    it("should add a toast", () => {
      const newToast = {
        id: "1",
        title: "Test Toast",
        description: "Test description",
        open: true,
      };

      const result = reducer(initialState, {
        type: "ADD_TOAST",
        toast: newToast,
      });

      expect(result.toasts).toHaveLength(1);
      expect(result.toasts[0].title).toBe("Test Toast");
    });

    it("should limit toasts to TOAST_LIMIT (1)", () => {
      const toast1 = { id: "1", title: "Toast 1", open: true };
      const toast2 = { id: "2", title: "Toast 2", open: true };

      let state = reducer(initialState, { type: "ADD_TOAST", toast: toast1 });
      state = reducer(state, { type: "ADD_TOAST", toast: toast2 });

      // Only the most recent toast should be kept
      expect(state.toasts).toHaveLength(1);
      expect(state.toasts[0].title).toBe("Toast 2");
    });

    it("should update a toast", () => {
      const existingToast = { id: "1", title: "Original", open: true };
      const state = { toasts: [existingToast] };

      const result = reducer(state, {
        type: "UPDATE_TOAST",
        toast: { id: "1", title: "Updated" },
      });

      expect(result.toasts[0].title).toBe("Updated");
    });

    it("should dismiss a specific toast", () => {
      const existingToast = { id: "1", title: "Toast", open: true };
      const state = { toasts: [existingToast] };

      const result = reducer(state, {
        type: "DISMISS_TOAST",
        toastId: "1",
      });

      expect(result.toasts[0].open).toBe(false);
    });

    it("should dismiss all toasts when no toastId provided", () => {
      const state = {
        toasts: [
          { id: "1", title: "Toast 1", open: true },
          { id: "2", title: "Toast 2", open: true },
        ],
      };

      const result = reducer(state, {
        type: "DISMISS_TOAST",
      });

      expect(result.toasts.every((t) => t.open === false)).toBe(true);
    });

    it("should remove a specific toast", () => {
      const state = {
        toasts: [
          { id: "1", title: "Toast 1", open: true },
          { id: "2", title: "Toast 2", open: true },
        ],
      };

      const result = reducer(state, {
        type: "REMOVE_TOAST",
        toastId: "1",
      });

      expect(result.toasts).toHaveLength(1);
      expect(result.toasts[0].id).toBe("2");
    });

    it("should remove all toasts when no toastId provided", () => {
      const state = {
        toasts: [
          { id: "1", title: "Toast 1", open: true },
          { id: "2", title: "Toast 2", open: true },
        ],
      };

      const result = reducer(state, {
        type: "REMOVE_TOAST",
      });

      expect(result.toasts).toHaveLength(0);
    });
  });

  describe("useToast hook", () => {
    it("should create a toast", () => {
      const { result } = renderHook(() => useToast());

      act(() => {
        result.current.toast({
          title: "Test Toast",
          description: "Test description",
        });
      });

      expect(result.current.toasts).toHaveLength(1);
      expect(result.current.toasts[0].title).toBe("Test Toast");
    });

    it("should dismiss a toast", () => {
      const { result } = renderHook(() => useToast());

      let toastId: string;
      act(() => {
        const toastResult = result.current.toast({
          title: "Test Toast",
        });
        toastId = toastResult.id;
      });

      act(() => {
        result.current.dismiss(toastId);
      });

      expect(result.current.toasts[0].open).toBe(false);
    });

    it("should return toast function that creates and returns control methods", () => {
      const { result } = renderHook(() => useToast());

      let toastControl: {
        id: string;
        dismiss: () => void;
        update: (props: any) => void;
      };
      act(() => {
        toastControl = result.current.toast({
          title: "Test Toast",
        });
      });

      expect(toastControl!.id).toBeDefined();
      expect(typeof toastControl!.dismiss).toBe("function");
      expect(typeof toastControl!.update).toBe("function");
    });

    it("should update a toast using returned update function", () => {
      const { result } = renderHook(() => useToast());

      let toastControl: {
        id: string;
        dismiss: () => void;
        update: (props: any) => void;
      };
      act(() => {
        toastControl = result.current.toast({
          title: "Original Title",
        });
      });

      act(() => {
        toastControl!.update({ title: "Updated Title" });
      });

      expect(result.current.toasts[0].title).toBe("Updated Title");
    });
  });

  describe("standalone toast function", () => {
    it("should create a toast with unique id", () => {
      let result1: { id: string };
      let result2: { id: string };

      act(() => {
        result1 = toast({ title: "Toast 1" });
        result2 = toast({ title: "Toast 2" });
      });

      expect(result1!.id).not.toBe(result2!.id);
    });
  });
});
