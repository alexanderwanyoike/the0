'use client';
import dynamic from 'next/dynamic';

const Plot = dynamic(() => import('react-plotly.js'), { ssr: false });

interface PlotModalProps {
  isOpen: boolean;
  onClose: () => void;
  plot: any;
}

const PlotModal: React.FC<PlotModalProps> = ({ isOpen, onClose, plot }) => {
  if (!isOpen) return null;

  return (
    <div className="fixed inset-0 bg-black bg-opacity-50 flex justify-center items-center z-50">
      <div className="bg-white dark:bg-gray-800 p-4 rounded-lg w-11/12 h-5/6 flex flex-col">
        <div className="flex justify-between items-center mb-4">
          <button
            onClick={onClose}
            className="text-gray-400 hover:text-gray-500 focus:outline-none"
          >
            <span className="sr-only">Close</span>
            <svg
              className="h-6 w-6"
              fill="none"
              viewBox="0 0 24 24"
              stroke="currentColor"
            >
              <path
                strokeLinecap="round"
                strokeLinejoin="round"
                strokeWidth={2}
                d="M6 18L18 6M6 6l12 12"
              />
            </svg>
          </button>
        </div>
        <div className="flex-grow">
          <Plot
            data={plot.data}
            layout={{
              ...plot.layout,
              autosize: true,
              margin: { l: 50, r: 50, t: 50, b: 50 },
            }}
            config={{ responsive: true }}
            style={{ width: '100%', height: '100%' }}
          />
        </div>
      </div>
    </div>
  );
};

export default PlotModal;
