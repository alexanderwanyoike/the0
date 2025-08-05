export type Operation = {
  type: "subscription_update_confirm" | "payment_method_update";
  operationData?: any;
};
