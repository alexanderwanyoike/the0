export function TypingIndicator() {
  return (
    <div className="flex items-center gap-1">
      <div className="flex space-x-1">
        <div className="w-2 h-2 bg-current rounded-full animate-pulse"></div>
        <div
          className="w-2 h-2 bg-current rounded-full animate-pulse"
          style={{ animationDelay: '0.2s' }}
        ></div>
        <div
          className="w-2 h-2 bg-current rounded-full animate-pulse"
          style={{ animationDelay: '0.4s' }}
        ></div>
      </div>
      <span className="ml-2 text-sm">the0 is thinking...</span>
    </div>
  )
}
