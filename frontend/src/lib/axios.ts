export const getErrorMessage = (error: any): string => {
  if (error.response) {
    return (
      error?.response?.data?.error?.message ||
      error?.response?.data?.message ||
      error?.response?.statusText ||
      error?.response?.message ||
      'Unknown error'
    );
  }

  if (error.request) {
    return 'No response received from server';
  }

  return `Error: ${error.message}`;
};

export const getErrorStatusCode = (error: any): number => {
  if (error.response) {
    return error.response.status;
  }

  if (error.request) {
    return 500; // No response received, consider it a server error
  }

  return 500; // Default to server error for other cases
};
