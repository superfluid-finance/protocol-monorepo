"use strict";
export const validateSuperTokenRequest = validate10;
const schema11 = {"$id":"validateSuperTokenRequest","type":"object","additionalProperties":false,"properties":{"isListed":{"type":"boolean","nullable":true}}};

function validate10(data, {instancePath="", parentData, parentDataProperty, rootData=data}={}){
/*# sourceURL="validateSuperTokenRequest" */;
let vErrors = null;
let errors = 0;
if(errors === 0){
if(data && typeof data == "object" && !Array.isArray(data)){
const _errs1 = errors;
for(const key0 in data){
if(!(key0 === "isListed")){
validate10.errors = [{instancePath,schemaPath:"#/additionalProperties",keyword:"additionalProperties",params:{additionalProperty: key0},message:"must NOT have additional properties"}];
return false;
break;
}
}
if(_errs1 === errors){
if(data.isListed !== undefined){
let data0 = data.isListed;
if((typeof data0 !== "boolean") && (data0 !== null)){
validate10.errors = [{instancePath:instancePath+"/isListed",schemaPath:"#/properties/isListed/type",keyword:"type",params:{type: "boolean"},message:"must be boolean"}];
return false;
}
}
}
}
else {
validate10.errors = [{instancePath,schemaPath:"#/type",keyword:"type",params:{type: "object"},message:"must be object"}];
return false;
}
}
validate10.errors = vErrors;
return errors === 0;
}

export const validateEventRequest = validate11;
const schema12 = {"$id":"validateEventRequest","type":"object","additionalProperties":false,"properties":{"account":{"type":"string","format":"addressOrEmpty","nullable":true},"timestamp_gt":{"type":"number","nullable":true}}};
const formats0 = /(^(0x)?[0-9a-fA-F]{40}$)?/;

function validate11(data, {instancePath="", parentData, parentDataProperty, rootData=data}={}){
/*# sourceURL="validateEventRequest" */;
let vErrors = null;
let errors = 0;
if(errors === 0){
if(data && typeof data == "object" && !Array.isArray(data)){
const _errs1 = errors;
for(const key0 in data){
if(!((key0 === "account") || (key0 === "timestamp_gt"))){
validate11.errors = [{instancePath,schemaPath:"#/additionalProperties",keyword:"additionalProperties",params:{additionalProperty: key0},message:"must NOT have additional properties"}];
return false;
break;
}
}
if(_errs1 === errors){
if(data.account !== undefined){
let data0 = data.account;
const _errs2 = errors;
if((typeof data0 !== "string") && (data0 !== null)){
validate11.errors = [{instancePath:instancePath+"/account",schemaPath:"#/properties/account/type",keyword:"type",params:{type: "string"},message:"must be string"}];
return false;
}
if(errors === _errs2){
if(errors === _errs2){
if(typeof data0 === "string"){
if(!(formats0.test(data0))){
validate11.errors = [{instancePath:instancePath+"/account",schemaPath:"#/properties/account/format",keyword:"format",params:{format: "addressOrEmpty"},message:"must match format \""+"addressOrEmpty"+"\""}];
return false;
}
}
}
}
var valid0 = _errs2 === errors;
}
else {
var valid0 = true;
}
if(valid0){
if(data.timestamp_gt !== undefined){
let data1 = data.timestamp_gt;
const _errs5 = errors;
if((!((typeof data1 == "number") && (isFinite(data1)))) && (data1 !== null)){
validate11.errors = [{instancePath:instancePath+"/timestamp_gt",schemaPath:"#/properties/timestamp_gt/type",keyword:"type",params:{type: "number"},message:"must be number"}];
return false;
}
var valid0 = _errs5 === errors;
}
else {
var valid0 = true;
}
}
}
}
else {
validate11.errors = [{instancePath,schemaPath:"#/type",keyword:"type",params:{type: "object"},message:"must be object"}];
return false;
}
}
validate11.errors = vErrors;
return errors === 0;
}

export const validateIndexRequest = validate12;
const schema13 = {"$id":"validateIndexRequest","type":"object","additionalProperties":false,"properties":{"indexId":{"type":"string","format":"stringInteger","nullable":true},"publisher":{"type":"string","format":"addressOrEmpty","nullable":true},"token":{"type":"string","format":"addressOrEmpty","nullable":true}}};
const formats2 = /\d+/;

function validate12(data, {instancePath="", parentData, parentDataProperty, rootData=data}={}){
/*# sourceURL="validateIndexRequest" */;
let vErrors = null;
let errors = 0;
if(errors === 0){
if(data && typeof data == "object" && !Array.isArray(data)){
const _errs1 = errors;
for(const key0 in data){
if(!(((key0 === "indexId") || (key0 === "publisher")) || (key0 === "token"))){
validate12.errors = [{instancePath,schemaPath:"#/additionalProperties",keyword:"additionalProperties",params:{additionalProperty: key0},message:"must NOT have additional properties"}];
return false;
break;
}
}
if(_errs1 === errors){
if(data.indexId !== undefined){
let data0 = data.indexId;
const _errs2 = errors;
if((typeof data0 !== "string") && (data0 !== null)){
validate12.errors = [{instancePath:instancePath+"/indexId",schemaPath:"#/properties/indexId/type",keyword:"type",params:{type: "string"},message:"must be string"}];
return false;
}
if(errors === _errs2){
if(errors === _errs2){
if(typeof data0 === "string"){
if(!(formats2.test(data0))){
validate12.errors = [{instancePath:instancePath+"/indexId",schemaPath:"#/properties/indexId/format",keyword:"format",params:{format: "stringInteger"},message:"must match format \""+"stringInteger"+"\""}];
return false;
}
}
}
}
var valid0 = _errs2 === errors;
}
else {
var valid0 = true;
}
if(valid0){
if(data.publisher !== undefined){
let data1 = data.publisher;
const _errs5 = errors;
if((typeof data1 !== "string") && (data1 !== null)){
validate12.errors = [{instancePath:instancePath+"/publisher",schemaPath:"#/properties/publisher/type",keyword:"type",params:{type: "string"},message:"must be string"}];
return false;
}
if(errors === _errs5){
if(errors === _errs5){
if(typeof data1 === "string"){
if(!(formats0.test(data1))){
validate12.errors = [{instancePath:instancePath+"/publisher",schemaPath:"#/properties/publisher/format",keyword:"format",params:{format: "addressOrEmpty"},message:"must match format \""+"addressOrEmpty"+"\""}];
return false;
}
}
}
}
var valid0 = _errs5 === errors;
}
else {
var valid0 = true;
}
if(valid0){
if(data.token !== undefined){
let data2 = data.token;
const _errs8 = errors;
if((typeof data2 !== "string") && (data2 !== null)){
validate12.errors = [{instancePath:instancePath+"/token",schemaPath:"#/properties/token/type",keyword:"type",params:{type: "string"},message:"must be string"}];
return false;
}
if(errors === _errs8){
if(errors === _errs8){
if(typeof data2 === "string"){
if(!(formats0.test(data2))){
validate12.errors = [{instancePath:instancePath+"/token",schemaPath:"#/properties/token/format",keyword:"format",params:{format: "addressOrEmpty"},message:"must match format \""+"addressOrEmpty"+"\""}];
return false;
}
}
}
}
var valid0 = _errs8 === errors;
}
else {
var valid0 = true;
}
}
}
}
}
else {
validate12.errors = [{instancePath,schemaPath:"#/type",keyword:"type",params:{type: "object"},message:"must be object"}];
return false;
}
}
validate12.errors = vErrors;
return errors === 0;
}

export const validateAccountTokenSnapshotRequest = validate13;
const schema14 = {"$id":"validateAccountTokenSnapshotRequest","type":"object","additionalProperties":false,"properties":{"account":{"type":"string","format":"addressOrEmpty","nullable":true},"token":{"type":"string","format":"addressOrEmpty","nullable":true}}};

function validate13(data, {instancePath="", parentData, parentDataProperty, rootData=data}={}){
/*# sourceURL="validateAccountTokenSnapshotRequest" */;
let vErrors = null;
let errors = 0;
if(errors === 0){
if(data && typeof data == "object" && !Array.isArray(data)){
const _errs1 = errors;
for(const key0 in data){
if(!((key0 === "account") || (key0 === "token"))){
validate13.errors = [{instancePath,schemaPath:"#/additionalProperties",keyword:"additionalProperties",params:{additionalProperty: key0},message:"must NOT have additional properties"}];
return false;
break;
}
}
if(_errs1 === errors){
if(data.account !== undefined){
let data0 = data.account;
const _errs2 = errors;
if((typeof data0 !== "string") && (data0 !== null)){
validate13.errors = [{instancePath:instancePath+"/account",schemaPath:"#/properties/account/type",keyword:"type",params:{type: "string"},message:"must be string"}];
return false;
}
if(errors === _errs2){
if(errors === _errs2){
if(typeof data0 === "string"){
if(!(formats0.test(data0))){
validate13.errors = [{instancePath:instancePath+"/account",schemaPath:"#/properties/account/format",keyword:"format",params:{format: "addressOrEmpty"},message:"must match format \""+"addressOrEmpty"+"\""}];
return false;
}
}
}
}
var valid0 = _errs2 === errors;
}
else {
var valid0 = true;
}
if(valid0){
if(data.token !== undefined){
let data1 = data.token;
const _errs5 = errors;
if((typeof data1 !== "string") && (data1 !== null)){
validate13.errors = [{instancePath:instancePath+"/token",schemaPath:"#/properties/token/type",keyword:"type",params:{type: "string"},message:"must be string"}];
return false;
}
if(errors === _errs5){
if(errors === _errs5){
if(typeof data1 === "string"){
if(!(formats0.test(data1))){
validate13.errors = [{instancePath:instancePath+"/token",schemaPath:"#/properties/token/format",keyword:"format",params:{format: "addressOrEmpty"},message:"must match format \""+"addressOrEmpty"+"\""}];
return false;
}
}
}
}
var valid0 = _errs5 === errors;
}
else {
var valid0 = true;
}
}
}
}
else {
validate13.errors = [{instancePath,schemaPath:"#/type",keyword:"type",params:{type: "object"},message:"must be object"}];
return false;
}
}
validate13.errors = vErrors;
return errors === 0;
}

export const validateIndexSubscriptionRequest = validate14;
const schema15 = {"$id":"validateIndexSubscriptionRequest","type":"object","additionalProperties":false,"properties":{"subscriber":{"type":"string","format":"stringInteger","nullable":true},"approved":{"type":"boolean","nullable":true}}};

function validate14(data, {instancePath="", parentData, parentDataProperty, rootData=data}={}){
/*# sourceURL="validateIndexSubscriptionRequest" */;
let vErrors = null;
let errors = 0;
if(errors === 0){
if(data && typeof data == "object" && !Array.isArray(data)){
const _errs1 = errors;
for(const key0 in data){
if(!((key0 === "subscriber") || (key0 === "approved"))){
validate14.errors = [{instancePath,schemaPath:"#/additionalProperties",keyword:"additionalProperties",params:{additionalProperty: key0},message:"must NOT have additional properties"}];
return false;
break;
}
}
if(_errs1 === errors){
if(data.subscriber !== undefined){
let data0 = data.subscriber;
const _errs2 = errors;
if((typeof data0 !== "string") && (data0 !== null)){
validate14.errors = [{instancePath:instancePath+"/subscriber",schemaPath:"#/properties/subscriber/type",keyword:"type",params:{type: "string"},message:"must be string"}];
return false;
}
if(errors === _errs2){
if(errors === _errs2){
if(typeof data0 === "string"){
if(!(formats2.test(data0))){
validate14.errors = [{instancePath:instancePath+"/subscriber",schemaPath:"#/properties/subscriber/format",keyword:"format",params:{format: "stringInteger"},message:"must match format \""+"stringInteger"+"\""}];
return false;
}
}
}
}
var valid0 = _errs2 === errors;
}
else {
var valid0 = true;
}
if(valid0){
if(data.approved !== undefined){
let data1 = data.approved;
const _errs5 = errors;
if((typeof data1 !== "boolean") && (data1 !== null)){
validate14.errors = [{instancePath:instancePath+"/approved",schemaPath:"#/properties/approved/type",keyword:"type",params:{type: "boolean"},message:"must be boolean"}];
return false;
}
var valid0 = _errs5 === errors;
}
else {
var valid0 = true;
}
}
}
}
else {
validate14.errors = [{instancePath,schemaPath:"#/type",keyword:"type",params:{type: "object"},message:"must be object"}];
return false;
}
}
validate14.errors = vErrors;
return errors === 0;
}

export const validateStreamRequest = validate15;
const schema16 = {"$id":"validateStreamRequest","type":"object","additionalProperties":false,"properties":{"sender":{"type":"string","format":"addressOrEmpty","nullable":true},"receiver":{"type":"string","format":"addressOrEmpty","nullable":true},"token":{"type":"string","format":"addressOrEmpty","nullable":true}}};

function validate15(data, {instancePath="", parentData, parentDataProperty, rootData=data}={}){
/*# sourceURL="validateStreamRequest" */;
let vErrors = null;
let errors = 0;
if(errors === 0){
if(data && typeof data == "object" && !Array.isArray(data)){
const _errs1 = errors;
for(const key0 in data){
if(!(((key0 === "sender") || (key0 === "receiver")) || (key0 === "token"))){
validate15.errors = [{instancePath,schemaPath:"#/additionalProperties",keyword:"additionalProperties",params:{additionalProperty: key0},message:"must NOT have additional properties"}];
return false;
break;
}
}
if(_errs1 === errors){
if(data.sender !== undefined){
let data0 = data.sender;
const _errs2 = errors;
if((typeof data0 !== "string") && (data0 !== null)){
validate15.errors = [{instancePath:instancePath+"/sender",schemaPath:"#/properties/sender/type",keyword:"type",params:{type: "string"},message:"must be string"}];
return false;
}
if(errors === _errs2){
if(errors === _errs2){
if(typeof data0 === "string"){
if(!(formats0.test(data0))){
validate15.errors = [{instancePath:instancePath+"/sender",schemaPath:"#/properties/sender/format",keyword:"format",params:{format: "addressOrEmpty"},message:"must match format \""+"addressOrEmpty"+"\""}];
return false;
}
}
}
}
var valid0 = _errs2 === errors;
}
else {
var valid0 = true;
}
if(valid0){
if(data.receiver !== undefined){
let data1 = data.receiver;
const _errs5 = errors;
if((typeof data1 !== "string") && (data1 !== null)){
validate15.errors = [{instancePath:instancePath+"/receiver",schemaPath:"#/properties/receiver/type",keyword:"type",params:{type: "string"},message:"must be string"}];
return false;
}
if(errors === _errs5){
if(errors === _errs5){
if(typeof data1 === "string"){
if(!(formats0.test(data1))){
validate15.errors = [{instancePath:instancePath+"/receiver",schemaPath:"#/properties/receiver/format",keyword:"format",params:{format: "addressOrEmpty"},message:"must match format \""+"addressOrEmpty"+"\""}];
return false;
}
}
}
}
var valid0 = _errs5 === errors;
}
else {
var valid0 = true;
}
if(valid0){
if(data.token !== undefined){
let data2 = data.token;
const _errs8 = errors;
if((typeof data2 !== "string") && (data2 !== null)){
validate15.errors = [{instancePath:instancePath+"/token",schemaPath:"#/properties/token/type",keyword:"type",params:{type: "string"},message:"must be string"}];
return false;
}
if(errors === _errs8){
if(errors === _errs8){
if(typeof data2 === "string"){
if(!(formats0.test(data2))){
validate15.errors = [{instancePath:instancePath+"/token",schemaPath:"#/properties/token/format",keyword:"format",params:{format: "addressOrEmpty"},message:"must match format \""+"addressOrEmpty"+"\""}];
return false;
}
}
}
}
var valid0 = _errs8 === errors;
}
else {
var valid0 = true;
}
}
}
}
}
else {
validate15.errors = [{instancePath,schemaPath:"#/type",keyword:"type",params:{type: "object"},message:"must be object"}];
return false;
}
}
validate15.errors = vErrors;
return errors === 0;
}
