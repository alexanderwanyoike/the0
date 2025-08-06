"use client";
interface TableProps {
  headers: string[];
  data: Record<string, any>[];
  keyField: string;
}

const CustomTable: React.FC<TableProps> = ({ headers, data, keyField }) => {
  return (
    <div className="overflow-x-auto rounded-lg shadow">
      <table className="w-full text-sm text-left text-gray-500 dark:text-gray-400">
        <thead className="text-xs text-gray-700 uppercase bg-gray-50 dark:bg-gray-700 dark:text-gray-400">
          <tr>
            {headers.map((header, index) => (
              <th key={index} scope="col" className="px-6 py-3">
                {header}
              </th>
            ))}
          </tr>
        </thead>
        <tbody>
          {data.map((row, rowIndex) => (
            <tr
              key={rowIndex}
              className={`bg-white border-b dark:bg-gray-800 dark:border-gray-700 ${rowIndex % 2 === 0 ? "bg-gray-50 dark:bg-gray-900" : ""}`}
            >
              {headers.map((header, cellIndex) => (
                <td
                  key={`${row[keyField]}-${cellIndex}`}
                  className="px-6 py-4 font-medium text-gray-900 whitespace-nowrap dark:text-white"
                >
                  {row[header.toLowerCase()]}
                </td>
              ))}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
};

export default CustomTable;
