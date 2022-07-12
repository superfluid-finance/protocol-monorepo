import Ajv, { JSONSchemaType, _ } from "ajv";
import standaloneCode from "ajv/dist/standalone";
import {
    IAccountTokenSnapshotFilter,
    IIndexRequestFilter,
    IIndexSubscriptionRequestFilter,
    IStreamRequestFilter,
    ISuperTokenRequestFilter,
} from "../src/interfaces";
import { writeFileSync } from "fs";
import { join } from "path";
import { IEventFilter } from "../src/events";

const superTokenRequestSchema: JSONSchemaType<ISuperTokenRequestFilter> = {
    $id: "validateSuperTokenRequest",
    type: "object",
    additionalProperties: false,
    properties: {
        isListed: { type: "boolean", nullable: true },
    },
};

const eventRequestSchema: JSONSchemaType<IEventFilter> = {
    $id: "validateEventRequest",
    type: "object",
    additionalProperties: false,
    properties: {
        account: { type: "string", format: "addressOrEmpty", nullable: true },
        timestamp_gt: {
            type: "number",
            nullable: true,
        },
    },
};

const indexRequestSchema: JSONSchemaType<IIndexRequestFilter> = {
    $id: "validateIndexRequest",
    type: "object",
    additionalProperties: false,
    properties: {
        indexId: { type: "string", format: "stringInteger", nullable: true },
        publisher: { type: "string", format: "addressOrEmpty", nullable: true },
        token: { type: "string", format: "addressOrEmpty", nullable: true },
    },
};

const accountTokenSnapshotRequestSchema: JSONSchemaType<IAccountTokenSnapshotFilter> =
    {
        $id: "validateAccountTokenSnapshotRequest",
        type: "object",
        additionalProperties: false,
        properties: {
            account: {
                type: "string",
                format: "addressOrEmpty",
                nullable: true,
            },
            token: { type: "string", format: "addressOrEmpty", nullable: true },
        },
    };

const indexSubscriptionRequestSchema: JSONSchemaType<IIndexSubscriptionRequestFilter> =
    {
        $id: "validateIndexSubscriptionRequest",
        type: "object",
        additionalProperties: false,
        properties: {
            subscriber: {
                type: "string",
                format: "stringInteger",
                nullable: true,
            },
            approved: { type: "boolean", nullable: true },
        },
    };

const streamRequestSchema: JSONSchemaType<IStreamRequestFilter> = {
    $id: "validateStreamRequest",
    type: "object",
    additionalProperties: false,
    properties: {
        sender: { type: "string", format: "addressOrEmpty", nullable: true },
        receiver: { type: "string", format: "addressOrEmpty", nullable: true },
        token: { type: "string", format: "addressOrEmpty", nullable: true },
    },
};

const ajv = new Ajv({
    schemas: [
        superTokenRequestSchema,
        eventRequestSchema,
        indexRequestSchema,
        accountTokenSnapshotRequestSchema,
        indexSubscriptionRequestSchema,
        streamRequestSchema,
    ],
    formats: require("./ajvCustomFormats.js"),
    code: {
        lines: true,
        optimize: true,
        source: true,
        esm: true,
        formats: _`require("./ajvCustomFormats.js")`,
    },
});

let moduleCode = standaloneCode(ajv);

// Now you can write the module code to file
writeFileSync(join(__dirname, "..", "./src/", "./ajvValidations.generated.js"), moduleCode);
