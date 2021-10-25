import Ajv, { JSONSchemaType } from "ajv";
import { ethers } from "ethers";
import {
    IIndexRequestFilter,
    IIndexSubscriptionRequestFilter,
    IPaginateRequest,
    IStreamRequestFilter,
} from "./interfaces";

const ajv = new Ajv();
ajv.addFormat("addressOrEmpty", {
    type: "string",
    validate: (x: string) => x === "" || ethers.utils.isAddress(x),
});
ajv.addFormat("stringNumber", {
    type: "string",
    validate: (x: string) => !isNaN(Number(x)),
});

// Schemas
const paginateSchema: JSONSchemaType<IPaginateRequest> = {
    type: "object",
    additionalProperties: false,
    properties: {
        first: { type: "number", nullable: true },
        skip: { type: "number", nullable: true },
    },
};

const indexRequestSchema: JSONSchemaType<IIndexRequestFilter> = {
    type: "object",
    additionalProperties: false,
    properties: {
        indexId: { type: "string", format: "stringNumber", nullable: true },
        publisher: { type: "string", format: "addressOrEmpty", nullable: true },
        token: { type: "string", format: "addressOrEmpty", nullable: true },
    },
};

const indexSubscriptionRequestSchema: JSONSchemaType<IIndexSubscriptionRequestFilter> =
    {
        type: "object",
        additionalProperties: false,
        properties: {
            subscriber: {
                type: "string",
                format: "stringNumber",
                nullable: true,
            },
            approved: { type: "boolean", nullable: true },
        },
    };

const streamRequestSchema: JSONSchemaType<IStreamRequestFilter> = {
    type: "object",
    additionalProperties: false,
    properties: {
        sender: { type: "string", format: "addressOrEmpty", nullable: true },
        receiver: { type: "string", format: "addressOrEmpty", nullable: true },
        token: { type: "string", format: "addressOrEmpty", nullable: true },
    },
};

// Validate functions
export const validateIndexRequest = ajv.compile(indexRequestSchema);
export const validateIndexSubscriptionRequest = ajv.compile(indexSubscriptionRequestSchema);
export const validatePaginateOptions = ajv.compile(paginateSchema);
export const validateStreamRequest = ajv.compile(streamRequestSchema);

// Validate function helper
export const handleValidatePaginate = (paginateOptions: IPaginateRequest) => {
    if (!validatePaginateOptions(paginateOptions)) {
        throw new Error(
            "Invalid paginate object - " +
                JSON.stringify(validatePaginateOptions.errors)
        );
    }
};
